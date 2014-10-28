module GraphDraw.MutableDraw where

import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V
import Control.Monad.ST
import Control.Monad
import Control.Monad.Primitive
import System.Random
import Data.List
import Control.Applicative
import Data.Graph.Inductive.Example
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive
import Control.Monad
import Data.Maybe
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

type Force    = (Double, Double)
type Location = (Double, Double)

tupleListZero :: Int -> [(Double,Double)]
tupleListZero n = getZipList $ (,) <$> ZipList (take n (repeat 0.0)) <*> ZipList (take n (repeat 0.0)) 

tupleListRand :: Int -> [(Double,Double)]
tupleListRand n = out
  where
    out       = getZipList $ (,) <$> ZipList (take n single) <*> ZipList (drop n single)
    single    = take (2*n) (randomRs (1,10) (mkStdGen 0))

giveVec :: (DynGraph gr) => gr a b -> Int -> V.Vector Location
giveVec graph n = runST $ do
	position  <- V.thaw pos1
	force     <- V.thaw for1
	updateGraph graph n position force
 	V.freeze position
 	where
 		pos1 = V.fromList (tupleListRand (noNodes  graph))
 		for1 = V.fromList (tupleListZero (noNodes  graph))

updateGraph :: (PrimMonad m , DynGraph gr) => gr c d -> Int -> 
              M.MVector (PrimState m) Location -> 
              M.MVector (PrimState m) Force -> m ()
updateGraph inGraph n pos force  = do 
  forM_ [0..n-1] $ \j -> do
    forceAtN inGraph pos force
    forM_ (nodes inGraph) $ \i -> do
      --forceAtN inGraph pos force i
      v <- M.read force (i-1)
      M.write pos (i-1) v
      M.write force (i-1) (0.0,0.0)

forceAtN :: ( DynGraph gr , PrimMonad m) => gr c d -> M.MVector (PrimState m) Location ->  
			M.MVector (PrimState m) Force -> m ()
forceAtN g pos force = do
	forM_ [1..noNodes g] $ \n -> do
--		M.write force (n-1) (0.0,0.0)
		forM_ (neighbors g n) $ \i -> do
			attForce pos force n i 2
		forM_ ((nodes g) \\ ((neighbors g n)++[n])) $ \j -> do
			attForce pos force j n 1


tupleAdd :: (Double,Double) -> (Double,Double) -> (Double,Double)
tupleAdd (x1,y1) (x2,y2) = (x1+x2,y1+y2)
        
attForce :: (PrimMonad m)=> M.MVector (PrimState m) Location -> M.MVector (PrimState m) Force -> 
			Int -> Int -> Int -> m ()
attForce pos force n1 n2 typeOf = do
  pos1 <- M.read pos (n1-1)
  pos2 <- M.read pos (n2-1)
  forcePre <- M.read force (n1-1)
  M.write force (n1-1) (tupleAdd forcePre (calforce typeOf pos1 pos2))

calforce :: Int -> Location -> Location -> Force
calforce typeOf pos1 pos2 = result
  where
    (x1,y1) = pos1
    (x2,y2) = pos2
    dist    = sqrt ((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
    d       = if (dist/=0)    then dist else 2
    effect  = if (typeOf==1)  then (-1*(2*log d))  else (1/sqrt d)
    result  = (effect*(x1-x2)/d,effect*(y1-y2)/d)

myGraph :: Gr [Char] ()
myGraph = mkGraph
          [(1, "A"), (2, "B"), (3, "C"), (4, "D"), (5, "E")]
          [(1,2, ()),(2,3, ()),(3,4, ()),(4,5, ()),(5,1,())]
