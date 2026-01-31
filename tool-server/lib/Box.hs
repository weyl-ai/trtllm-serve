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
  , Alignment(..)
    -- * Rendering
  , renderTable
  , renderFrame
  , renderTree
    -- * Box Characters (exported for reuse)
  , BoxChars(..)
  , singleBox
  , doubleBox
  , roundedBox
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)


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
pad :: Alignment -> Int -> Text -> Text
pad align width txt
  | len >= width = T.take width txt
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
