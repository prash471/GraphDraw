module Main where

import GraphDraw.TestFgl
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.List

myGraph :: Gr [Char] ()
myGraph = mkGraph
          [(1, "A"), (2, "B"), (3, "C"), (4, "D"), (5, "E"), (6, "E"), (7, "E")]
          [(1,2, ()),(2,3, ()),(3,4, ()),(4,5, ()),(5,6, ()),(6,7, ())]
--________________________________________________________________________________________________________________________________
{-
myGraph = mkGraph
          [(1, "A"), (2, "B"), (3, "C"), (4, "D"), (5, "E")]
          [(1,2, ()),(2,3, ()),(3,4, ()),(4,2, ()),(1,3, ()),(1,4, ()),(1,5, ()),(2,5, ()),(3,5, ())]
-}
{-
myGraph = mkGraph
          [(1, "A"), (2, "B"), (3, "C"), (4, "D"), (5, "E"), (6, "F"), (7, "G")]
          [(2,1, ()),(2,3, ()),(2,4, ()),(2,5, ()),(2,6, ()),(2,7, ())]
  
-}
{-
myGraph = mkGraph
          [(1, "A"), (2, "B"), (3, "C"), (4, "D"), (5, "E")]
          [(1,2, ()),(2,3, ()),(3,4, ()),(4,5, ()),(5,1,()),(1,3,())]
-}
--_________________________________________________________________________________________________________________________________

node :: Int -> Diagram B R2
node n = text (show n) # scale 2.3 # fc white
       <> circle 2.3 # fc green # named n

arrowOpts = with Diagrams.Prelude.& arrowHead .~ dart Diagrams.Prelude.& headSize .~ 1.2
                 Diagrams.Prelude.& shaftStyle %~ lw 0.2

example :: Diagram B R2
example = position . flip zip (map node (nodes myGraph)) . map p2 $ (giveList myGraph 50)

main = mainWith (example # applyAll (map (\(x,y) -> connectOutside' arrowOpts x y ) (edges myGraph)) ::Diagram B R2)
