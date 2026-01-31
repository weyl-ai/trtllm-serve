{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Box Drawing - Dialed Once, The Result Is Saved
--
-- Pure functions for perfect Unicode box drawings.
-- No IO. No state. Same input, same output, forever.
--
-- @since 0.1.0

module Box
  ( -- * Box Types
    BoxStyle(..)
  , TableData(..)
  , FrameData(..)
  , TreeNode(..)
  , DiagramData(..)
  , DiagramNode(..)
  , DiagramEdge(..)
  , DiagramLayout(..)
  , Alignment(..)
    -- * Rendering
  , renderTable
  , renderFrame
  , renderTree
  , renderDiagram
    -- * Helpers (for testing)
  , pad
    -- * Box Characters (exported for reuse)
  , BoxChars(..)
  , singleBox
  , doubleBox
  , roundedBox
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map.Strict as M


-- ════════════════════════════════════════════════════════════════════════════════
-- Box Characters - Correct, Once, Forever
-- ════════════════════════════════════════════════════════════════════════════════

data BoxChars = BoxChars
  { bcTopLeft     :: !Char
  , bcTopRight    :: !Char
  , bcBottomLeft  :: !Char
  , bcBottomRight :: !Char
  , bcHorizontal  :: !Char
  , bcVertical    :: !Char
  , bcLeftTee     :: !Char
  , bcRightTee    :: !Char
  , bcTopTee      :: !Char
  , bcBottomTee   :: !Char
  , bcCross       :: !Char
  } deriving (Show, Eq)

-- | Single-line box characters
singleBox :: BoxChars
singleBox = BoxChars
  { bcTopLeft     = '┌'
  , bcTopRight    = '┐'
  , bcBottomLeft  = '└'
  , bcBottomRight = '┘'
  , bcHorizontal  = '─'
  , bcVertical    = '│'
  , bcLeftTee     = '├'
  , bcRightTee    = '┤'
  , bcTopTee      = '┬'
  , bcBottomTee   = '┴'
  , bcCross       = '┼'
  }

-- | Double-line box characters
doubleBox :: BoxChars
doubleBox = BoxChars
  { bcTopLeft     = '╔'
  , bcTopRight    = '╗'
  , bcBottomLeft  = '╚'
  , bcBottomRight = '╝'
  , bcHorizontal  = '═'
  , bcVertical    = '║'
  , bcLeftTee     = '╠'
  , bcRightTee    = '╣'
  , bcTopTee      = '╦'
  , bcBottomTee   = '╩'
  , bcCross       = '╬'
  }

-- | Rounded box characters
roundedBox :: BoxChars
roundedBox = BoxChars
  { bcTopLeft     = '╭'
  , bcTopRight    = '╮'
  , bcBottomLeft  = '╰'
  , bcBottomRight = '╯'
  , bcHorizontal  = '─'
  , bcVertical    = '│'
  , bcLeftTee     = '├'
  , bcRightTee    = '┤'
  , bcTopTee      = '┬'
  , bcBottomTee   = '┴'
  , bcCross       = '┼'
  }


-- ════════════════════════════════════════════════════════════════════════════════
-- Types
-- ════════════════════════════════════════════════════════════════════════════════

data BoxStyle = Single | Double | Rounded
  deriving (Show, Eq)

data Alignment = AlignLeft | AlignCenter | AlignRight
  deriving (Show, Eq)

data TableData = TableData
  { tdHeaders :: ![Text]
  , tdRows    :: ![[Text]]
  , tdStyle   :: !BoxStyle
  } deriving (Show, Eq)

data FrameData = FrameData
  { fdTitle   :: !(Maybe Text)
  , fdContent :: !Text
  , fdWidth   :: !(Maybe Int)
  , fdStyle   :: !BoxStyle
  } deriving (Show, Eq)

data TreeNode = TreeNode
  { tnLabel    :: !Text
  , tnChildren :: ![TreeNode]
  } deriving (Show, Eq)

-- | A node in a diagram
data DiagramNode = DiagramNode
  { dnId    :: !Text
  , dnLabel :: !Text
  } deriving (Show, Eq)

-- | An edge connecting two nodes
data DiagramEdge = DiagramEdge
  { deFrom  :: !Text
  , deTo    :: !Text
  , deLabel :: !(Maybe Text)
  } deriving (Show, Eq)

-- | Layout direction for diagram
data DiagramLayout = Horizontal | Vertical | Flow
  deriving (Show, Eq)

-- | Complete diagram specification
data DiagramData = DiagramData
  { ddNodes  :: ![DiagramNode]
  , ddEdges  :: ![DiagramEdge]
  , ddLayout :: !DiagramLayout
  , ddStyle  :: !BoxStyle
  } deriving (Show, Eq)


-- ════════════════════════════════════════════════════════════════════════════════
-- Helpers - Pure, Tested, Correct
-- ════════════════════════════════════════════════════════════════════════════════

-- | Get box characters for a style
boxChars :: BoxStyle -> BoxChars
boxChars Single  = singleBox
boxChars Double  = doubleBox
boxChars Rounded = roundedBox

-- | Text length (handles Unicode correctly)
textLen :: Text -> Int
textLen = T.length

-- | Pad text to width with alignment
-- Output is max(width, textLen txt) characters - never truncates
pad :: Alignment -> Int -> Text -> Text
pad align width txt
  | len >= width = txt  -- Never truncate, just return original
  | otherwise = case align of
      AlignLeft   -> txt <> T.replicate diff " "
      AlignRight  -> T.replicate diff " " <> txt
      AlignCenter -> T.replicate leftPad " " <> txt <> T.replicate rightPad " "
  where
    len = textLen txt
    diff = width - len
    leftPad = diff `div` 2
    rightPad = diff - leftPad

-- | Replicate a character as Text
rep :: Int -> Char -> Text
rep n c = T.replicate n (T.singleton c)


-- ════════════════════════════════════════════════════════════════════════════════
-- Table Rendering
-- ════════════════════════════════════════════════════════════════════════════════

-- | Render a table with headers and rows
--
-- >>> renderTable (TableData ["Name", "Age"] [["Alice", "30"], ["Bob", "25"]] Single)
-- ┌───────┬─────┐
-- │ Name  │ Age │
-- ├───────┼─────┤
-- │ Alice │ 30  │
-- │ Bob   │ 25  │
-- └───────┴─────┘
renderTable :: TableData -> Text
renderTable TableData{..} = T.unlines $ 
  [topBorder] ++ [headerRow] ++ [headerSep] ++ dataRows ++ [bottomBorder]
  where
    bc = boxChars tdStyle
    
    -- Calculate column widths (max of header and all rows, plus padding)
    colWidths :: [Int]
    colWidths = zipWith max headerWidths maxRowWidths
      where
        headerWidths = map ((+ 2) . textLen) tdHeaders
        maxRowWidths = 
          if null tdRows 
            then map (const 0) tdHeaders
            else map (maximum . map ((+ 2) . textLen)) (transpose tdRows)
    
    -- Transpose rows to get columns
    transpose :: [[a]] -> [[a]]
    transpose [] = []
    transpose ([] : _) = []
    transpose rows = map head rows : transpose (map tail rows)
    
    -- Border lines
    topBorder = T.singleton (bcTopLeft bc) 
      <> T.intercalate (T.singleton $ bcTopTee bc) (map (`rep` bcHorizontal bc) colWidths)
      <> T.singleton (bcTopRight bc)
    
    headerSep = T.singleton (bcLeftTee bc)
      <> T.intercalate (T.singleton $ bcCross bc) (map (`rep` bcHorizontal bc) colWidths)
      <> T.singleton (bcRightTee bc)
    
    bottomBorder = T.singleton (bcBottomLeft bc)
      <> T.intercalate (T.singleton $ bcBottomTee bc) (map (`rep` bcHorizontal bc) colWidths)
      <> T.singleton (bcBottomRight bc)
    
    -- Header row (centered)
    headerRow = T.singleton (bcVertical bc)
      <> T.intercalate (T.singleton $ bcVertical bc) 
           (zipWith (pad AlignCenter) colWidths tdHeaders)
      <> T.singleton (bcVertical bc)
    
    -- Data rows (left-aligned with leading space)
    dataRows = map mkRow tdRows
    mkRow cells = T.singleton (bcVertical bc)
      <> T.intercalate (T.singleton $ bcVertical bc)
           (zipWith (\w c -> " " <> pad AlignLeft (w - 1) c) colWidths (cells ++ repeat ""))
      <> T.singleton (bcVertical bc)


-- ════════════════════════════════════════════════════════════════════════════════
-- Frame Rendering
-- ════════════════════════════════════════════════════════════════════════════════

-- | Render a frame around content, optionally with title
--
-- >>> renderFrame (FrameData (Just "Title") "Hello\nWorld" Nothing Single)
-- ┌─ Title ─┐
-- │Hello    │
-- │World    │
-- └─────────┘
renderFrame :: FrameData -> Text
renderFrame FrameData{..} = T.unlines $ [topBorder] ++ contentLines ++ [bottomBorder]
  where
    bc = boxChars fdStyle
    lines_ = T.lines fdContent
    
    -- Calculate inner width
    maxContentWidth = maximum (0 : map textLen lines_)
    titleWidth = maybe 0 ((+ 2) . textLen) fdTitle  -- +2 for spaces around title
    innerWidth = max (max maxContentWidth titleWidth) (maybe 0 id fdWidth)
    
    -- Top border (with optional title)
    topBorder = case fdTitle of
      Nothing -> T.singleton (bcTopLeft bc) 
        <> rep innerWidth (bcHorizontal bc)
        <> T.singleton (bcTopRight bc)
      Just t -> 
        let titlePad = innerWidth - titleWidth
            leftPad = titlePad `div` 2
            rightPad = titlePad - leftPad
        in T.singleton (bcTopLeft bc)
           <> rep leftPad (bcHorizontal bc)
           <> " " <> t <> " "
           <> rep rightPad (bcHorizontal bc)
           <> T.singleton (bcTopRight bc)
    
    -- Bottom border
    bottomBorder = T.singleton (bcBottomLeft bc)
      <> rep innerWidth (bcHorizontal bc)
      <> T.singleton (bcBottomRight bc)
    
    -- Content lines
    contentLines = map mkLine lines_
    mkLine l = T.singleton (bcVertical bc) 
      <> pad AlignLeft innerWidth l 
      <> T.singleton (bcVertical bc)


-- ════════════════════════════════════════════════════════════════════════════════
-- Tree Rendering
-- ════════════════════════════════════════════════════════════════════════════════

-- | Render a tree structure
--
-- >>> renderTree (TreeNode "root" [TreeNode "child1" [], TreeNode "child2" [TreeNode "grandchild" []]])
-- root
-- ├── child1
-- └── child2
--     └── grandchild
renderTree :: TreeNode -> Text
renderTree node = T.unlines $ tnLabel node : renderChildren "" (tnChildren node)
  where
    renderChildren :: Text -> [TreeNode] -> [Text]
    renderChildren _ [] = []
    renderChildren prefix [child] = 
      -- Last child
      (prefix <> "└── " <> tnLabel child) 
      : renderChildren (prefix <> "    ") (tnChildren child)
    renderChildren prefix (child:rest) = 
      -- Not last child
      (prefix <> "├── " <> tnLabel child)
      : renderChildren (prefix <> "│   ") (tnChildren child)
      ++ renderChildren prefix rest


-- ════════════════════════════════════════════════════════════════════════════════
-- Diagram Rendering - Boxes with Arrows
-- ════════════════════════════════════════════════════════════════════════════════

-- | Arrow characters
data ArrowChars = ArrowChars
  { acRight     :: !Text  -- →
  , acLeft      :: !Text  -- ←
  , acDown      :: !Text  -- ↓
  , acUp        :: !Text  -- ↑
  , acHoriz     :: !Char  -- ─
  , acVert      :: !Char  -- │
  , acCornerDR  :: !Char  -- ┌ (down-right)
  , acCornerDL  :: !Char  -- ┐ (down-left)
  , acCornerUR  :: !Char  -- └ (up-right)
  , acCornerUL  :: !Char  -- ┘ (up-left)
  }

defaultArrows :: ArrowChars
defaultArrows = ArrowChars
  { acRight    = "→"
  , acLeft     = "←"
  , acDown     = "↓"
  , acUp       = "↑"
  , acHoriz    = '─'
  , acVert     = '│'
  , acCornerDR = '┌'
  , acCornerDL = '┐'
  , acCornerUR = '└'
  , acCornerUL = '┘'
  }

-- | Render a diagram with boxes and arrows
--
-- Horizontal layout:
-- ┌───────┐     ┌───────┐     ┌───────┐
-- │ Input │────→│Process│────→│Output │
-- └───────┘     └───────┘     └───────┘
--
-- Vertical layout:
-- ┌───────┐
-- │ Start │
-- └───┬───┘
--     │
--     ↓
-- ┌───────┐
-- │  End  │
-- └───────┘
renderDiagram :: DiagramData -> Text
renderDiagram dd@DiagramData{..} = case ddLayout of
  Horizontal -> renderHorizontal dd
  Vertical   -> renderVertical dd
  Flow       -> renderFlow dd

-- | Render boxes horizontally with arrows between them
renderHorizontal :: DiagramData -> Text
renderHorizontal DiagramData{..} = T.unlines result
  where
    bc = boxChars ddStyle
    ac = defaultArrows
    
    -- Calculate box dimensions
    boxWidth = maximum (6 : map (textLen . dnLabel) ddNodes) + 4
    boxHeight = 3
    arrowLen = 5
    
    -- Build edge map: from -> [(to, label)]
    edgeMap :: M.Map Text [(Text, Maybe Text)]
    edgeMap = M.fromListWith (++) 
      [(deFrom e, [(deTo e, deLabel e)]) | e <- ddEdges]
    
    -- Order nodes by edges (simple: keep original order)
    nodes = ddNodes
    
    -- Generate the three lines of a horizontal diagram
    result = 
      [ topLine, midLine, botLine ]
    
    -- Top line: ┌───────┐     ┌───────┐
    topLine = T.intercalate (T.replicate arrowLen " ") $
      map (\_ -> T.singleton (bcTopLeft bc) <> rep (boxWidth - 2) (bcHorizontal bc) <> T.singleton (bcTopRight bc)) nodes
    
    -- Middle line: │ Label │────→│ Label │
    midLine = T.concat $ zipWith renderNodeWithArrow [0..] nodes
    
    renderNodeWithArrow :: Int -> DiagramNode -> Text
    renderNodeWithArrow idx node =
      let content = T.singleton (bcVertical bc) <> pad AlignCenter (boxWidth - 2) (dnLabel node) <> T.singleton (bcVertical bc)
          -- Check if there's an arrow to the next node
          hasArrow = idx < length nodes - 1 && 
                     any (\(to, _) -> to == dnId (nodes !! (idx + 1))) 
                         (fromMaybe [] $ M.lookup (dnId node) edgeMap)
          arrow = if hasArrow 
                  then rep (arrowLen - 1) (acHoriz ac) <> acRight ac
                  else T.replicate arrowLen " "
      in if idx < length nodes - 1 
         then content <> arrow 
         else content
    
    -- Bottom line: └───────┘     └───────┘
    botLine = T.intercalate (T.replicate arrowLen " ") $
      map (\_ -> T.singleton (bcBottomLeft bc) <> rep (boxWidth - 2) (bcHorizontal bc) <> T.singleton (bcBottomRight bc)) nodes

-- | Render boxes vertically with arrows between them
renderVertical :: DiagramData -> Text
renderVertical DiagramData{..} = T.unlines $ concatMap renderNodeWithConnection (zip [0..] ddNodes)
  where
    bc = boxChars ddStyle
    ac = defaultArrows
    
    -- Calculate box dimensions
    boxWidth = maximum (6 : map (textLen . dnLabel) ddNodes) + 4
    
    -- Build edge map
    edgeMap :: M.Map Text [(Text, Maybe Text)]
    edgeMap = M.fromListWith (++) 
      [(deFrom e, [(deTo e, deLabel e)]) | e <- ddEdges]
    
    nodes = ddNodes
    centerPad = (boxWidth - 1) `div` 2
    
    renderNodeWithConnection :: (Int, DiagramNode) -> [Text]
    renderNodeWithConnection (idx, node) =
      let top = T.singleton (bcTopLeft bc) <> rep (boxWidth - 2) (bcHorizontal bc) <> T.singleton (bcTopRight bc)
          mid = T.singleton (bcVertical bc) <> pad AlignCenter (boxWidth - 2) (dnLabel node) <> T.singleton (bcVertical bc)
          -- Check if there's an arrow to the next node  
          hasArrow = idx < length nodes - 1 &&
                     any (\(to, _) -> to == dnId (nodes !! (idx + 1)))
                         (fromMaybe [] $ M.lookup (dnId node) edgeMap)
          bot = if hasArrow
                then T.singleton (bcBottomLeft bc) <> rep centerPad (bcHorizontal bc) <> "┬" <> rep (boxWidth - 3 - centerPad) (bcHorizontal bc) <> T.singleton (bcBottomRight bc)
                else T.singleton (bcBottomLeft bc) <> rep (boxWidth - 2) (bcHorizontal bc) <> T.singleton (bcBottomRight bc)
          arrow = if hasArrow
                  then [ T.replicate centerPad " " <> T.singleton (acVert ac)
                       , T.replicate centerPad " " <> acDown ac
                       ]
                  else []
      in [top, mid, bot] ++ arrow

-- | Flow layout: nodes arranged in rows with wrapping
renderFlow :: DiagramData -> Text
renderFlow DiagramData{..} = T.unlines result
  where
    bc = boxChars ddStyle
    ac = defaultArrows
    
    -- Calculate box dimensions  
    boxWidth = maximum (6 : map (textLen . dnLabel) ddNodes) + 4
    arrowLen = 3
    nodesPerRow = 4  -- Wrap after 4 nodes
    
    -- Build edge map
    edgeMap :: M.Map Text [(Text, Maybe Text)]
    edgeMap = M.fromListWith (++) 
      [(deFrom e, [(deTo e, deLabel e)]) | e <- ddEdges]
    
    -- Chunk nodes into rows
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)
    
    rows = chunksOf nodesPerRow ddNodes
    
    -- Render each row
    result = concatMap renderRow (zip [0..] rows)
    
    renderRow :: (Int, [DiagramNode]) -> [Text]
    renderRow (rowIdx, rowNodes) =
      let top = T.intercalate (T.replicate arrowLen " ") $
                  map (\_ -> T.singleton (bcTopLeft bc) <> rep (boxWidth - 2) (bcHorizontal bc) <> T.singleton (bcTopRight bc)) rowNodes
          mid = T.concat $ zipWith (renderNodeInRow rowIdx (length rowNodes)) [0..] rowNodes
          bot = T.intercalate (T.replicate arrowLen " ") $
                  map (\_ -> T.singleton (bcBottomLeft bc) <> rep (boxWidth - 2) (bcHorizontal bc) <> T.singleton (bcBottomRight bc)) rowNodes
          -- Add vertical arrow to next row if exists
          hasNextRow = rowIdx < length rows - 1
          rowArrow = if hasNextRow
                     then [ T.replicate ((boxWidth + arrowLen) * length rowNodes `div` 2 - 1) " " <> T.singleton (acVert ac)
                          , T.replicate ((boxWidth + arrowLen) * length rowNodes `div` 2 - 1) " " <> acDown ac
                          ]
                     else []
      in [top, mid, bot] ++ rowArrow
    
    renderNodeInRow :: Int -> Int -> Int -> DiagramNode -> Text
    renderNodeInRow rowIdx rowLen colIdx node =
      let content = T.singleton (bcVertical bc) <> pad AlignCenter (boxWidth - 2) (dnLabel node) <> T.singleton (bcVertical bc)
          -- Arrow to next in row
          nextInRow = colIdx < rowLen - 1
          arrow = if nextInRow
                  then rep (arrowLen - 1) (acHoriz ac) <> acRight ac
                  else ""
      in content <> arrow
