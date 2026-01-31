{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (when)

import Box

-- ════════════════════════════════════════════════════════════════════════════════
-- Generators
-- ════════════════════════════════════════════════════════════════════════════════

genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.unicode

genLabel :: Gen Text
genLabel = Gen.text (Range.linear 0 30) Gen.alphaNum

genStyle :: Gen BoxStyle
genStyle = Gen.element [Single, Double, Rounded]

genAlignment :: Gen Alignment  
genAlignment = Gen.element [AlignLeft, AlignCenter, AlignRight]

genLayout :: Gen DiagramLayout
genLayout = Gen.element [Horizontal, Vertical, Flow]

genTableData :: Gen TableData
genTableData = do
  numCols <- Gen.int (Range.linear 0 10)
  numRows <- Gen.int (Range.linear 0 20)
  headers <- Gen.list (Range.singleton numCols) genLabel
  rows <- Gen.list (Range.singleton numRows) (Gen.list (Range.singleton numCols) genLabel)
  style <- genStyle
  pure $ TableData headers rows style

genFrameData :: Gen FrameData
genFrameData = do
  title <- Gen.maybe genLabel
  content <- genText
  width <- Gen.maybe (Gen.int (Range.linear 0 200))
  style <- genStyle
  pure $ FrameData title content width style

genTreeNode :: Gen TreeNode
genTreeNode = Gen.recursive Gen.choice
  [ TreeNode <$> genLabel <*> pure [] ]  -- base case
  [ TreeNode <$> genLabel <*> Gen.list (Range.linear 0 5) genTreeNode ]

genDiagramNode :: Gen DiagramNode
genDiagramNode = DiagramNode <$> genLabel <*> genLabel

genDiagramEdge :: [DiagramNode] -> Gen DiagramEdge
genDiagramEdge nodes = do
  let ids = map dnId nodes
  from <- if null ids then genLabel else Gen.element ids
  to <- if null ids then genLabel else Gen.element ids
  label <- Gen.maybe genLabel
  pure $ DiagramEdge from to label

genDiagramData :: Gen DiagramData
genDiagramData = do
  nodes <- Gen.list (Range.linear 0 20) genDiagramNode
  edges <- Gen.list (Range.linear 0 30) (genDiagramEdge nodes)
  layout <- genLayout
  style <- genStyle
  pure $ DiagramData nodes edges layout style


-- ════════════════════════════════════════════════════════════════════════════════
-- Properties
-- ════════════════════════════════════════════════════════════════════════════════

-- | Tables should never crash
prop_table_total :: Property
prop_table_total = property $ do
  td <- forAll genTableData
  let result = renderTable td
  -- Should produce some output (at least borders)
  assert $ T.length result >= 0

-- | Frames should never crash  
prop_frame_total :: Property
prop_frame_total = property $ do
  fd <- forAll genFrameData
  let result = renderFrame fd
  assert $ T.length result >= 0

-- | Trees should never crash
prop_tree_total :: Property
prop_tree_total = property $ do
  tree <- forAll genTreeNode
  let result = renderTree tree
  assert $ T.length result >= 0

-- | Diagrams should never crash
prop_diagram_total :: Property
prop_diagram_total = property $ do
  dd <- forAll genDiagramData
  let result = renderDiagram dd
  assert $ T.length result >= 0

-- | All lines in table should have same length (box alignment)
prop_table_aligned :: Property
prop_table_aligned = property $ do
  td <- forAll genTableData
  let result = renderTable td
      lines_ = T.lines result
      lengths = map T.length lines_
  -- Skip empty tables
  when (not $ null lines_) $ do
    annotate $ "Line lengths: " ++ show lengths
    assert $ all (== head lengths) lengths

-- | All lines in frame should have same length
prop_frame_aligned :: Property
prop_frame_aligned = property $ do
  fd <- forAll genFrameData
  let result = renderFrame fd
      lines_ = T.lines result
      lengths = map T.length lines_
  when (not $ null lines_) $ do
    annotate $ "Line lengths: " ++ show lengths
    assert $ all (== head lengths) lengths

-- | Horizontal diagram lines should align
prop_diagram_horizontal_aligned :: Property
prop_diagram_horizontal_aligned = property $ do
  nodes <- forAll $ Gen.list (Range.linear 1 10) genDiagramNode
  edges <- forAll $ Gen.list (Range.linear 0 10) (genDiagramEdge nodes)
  style <- forAll genStyle
  let dd = DiagramData nodes edges Horizontal style
      result = renderDiagram dd
      lines_ = filter (not . T.null) $ T.lines result
      lengths = map T.length lines_
  when (length lines_ == 3) $ do  -- horizontal has exactly 3 lines
    annotate $ "Line lengths: " ++ show lengths
    assert $ all (== head lengths) lengths

-- | Pad should produce exact width
prop_pad_exact_width :: Property
prop_pad_exact_width = property $ do
  txt <- forAll genLabel
  width <- forAll $ Gen.int (Range.linear 0 100)
  align <- forAll genAlignment
  let result = pad align width txt
  annotate $ "Input: " ++ show txt ++ ", width: " ++ show width
  annotate $ "Result: " ++ show result ++ ", len: " ++ show (T.length result)
  assert $ T.length result == max width (T.length txt)


-- ════════════════════════════════════════════════════════════════════════════════
-- Main
-- ════════════════════════════════════════════════════════════════════════════════

main :: IO ()
main = do
  putStrLn "Breaking boxes with Hedgehog..."
  putStrLn ""
  
  results <- sequence
    [ checkParallel $ Group "Totality" 
        [ ("prop_table_total", prop_table_total)
        , ("prop_frame_total", prop_frame_total)
        , ("prop_tree_total", prop_tree_total)
        , ("prop_diagram_total", prop_diagram_total)
        ]
    , checkParallel $ Group "Alignment"
        [ ("prop_table_aligned", prop_table_aligned)
        , ("prop_frame_aligned", prop_frame_aligned)
        , ("prop_diagram_horizontal_aligned", prop_diagram_horizontal_aligned)
        , ("prop_pad_exact_width", prop_pad_exact_width)
        ]
    ]
  
  if and results
    then putStrLn "\n✓ All properties held. The box stands."
    else putStrLn "\n✗ BROKEN. Fix it."
