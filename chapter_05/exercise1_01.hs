module Exercises1_01 (fill) where

import Prettify
import Prelude hiding ((<>))

fill :: Int -> Doc -> Doc
fill columnWidth = fill' columnWidth
  where
    fill' cols Empty = Text (replicate (columnWidth - cols) ' ') <> Empty
    fill' cols (Char s) = Text (replicate (columnWidth - cols - 1) ' ') <> Char s
    fill' cols (Text t) = Text (replicate (columnWidth - cols - length t) ' ') <> Text t
    fill' cols Line = Text (replicate (columnWidth - cols) ' ') <> Line
    fill' cols (Concat Empty right) = Empty <> fill' cols right
    fill' cols (Concat (Char s) right) = Char s <> fill' (cols - 1) right
    fill' cols (Concat (Text t) right) = Text t <> fill' (length t - 1) right
    fill' cols (Concat Line right) = fill' cols Line <> fill' columnWidth right
    fill' cols (Concat (Concat doc1 doc2) right) = undefined
    fill' cols (Concat doc1 doc2) = doc1 `Concat` fill' cols doc2
    fill' cols (Union doc1 doc2) = Union (fill' cols doc1) (fill' cols doc2)
