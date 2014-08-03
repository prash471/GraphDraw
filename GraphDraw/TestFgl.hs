module GraphDraw.TestFgl where

import Data.Graph.Inductive.Example
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive
import System.Random 
import Control.Monad
import Data.List
import Data.Maybe
import System.IO.Unsafe
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

initGraph :: a -> ((Double,Double),(Double,Double))
initGraph x = ((0.0,0.0),(lx,ly))
  where
    (x,y) = unsafePerformIO randomPos
    (lx,ly) = (fromIntegral x/1.0 , fromIntegral y/1.0)

randomList :: Int -> StdGen -> [Int]
randomList n = take n . unfoldr (Just . random)

randomPos = do
    seed1 <- newStdGen
    seed2 <- newStdGen
    let [l1] = randomList 1 seed1
    let [l2] = randomList 1 seed2
    let ps = (fromIntegral (l1 `mod` 50), fromIntegral (l2 `mod` 50))
    return ps
  
funGraphMap :: DynGraph gr => a -> gr ((Double,Double),(Double,Double)) b-> Int ->((Double,Double),(Double,Double))
funGraphMap old g n = ((fx,fy),(lx,ly))
  where
    neighborNodes = neighbors g n
    nonNeighborNodes = (nodes g) \\ neighborNodes
    attF = foldr tupleAdd (0.0,0.0)(map (\x -> attForce g n x 2)  neighborNodes)
    repF = foldr tupleAdd (0.0,0.0)(map (\x -> attForce g x n 1)  nonNeighborNodes)
    (fx,fy) = tupleAdd attF repF
    Just ((fx1,fy1),(lx,ly)) = lab g n
--    (lx,ly) = (3.5,3.9)

tupleAdd :: (Double,Double) -> (Double,Double) -> (Double,Double)
tupleAdd (x1,y1) (x2,y2) = (x1+x2,y1+y2)

nmapMod :: DynGraph gr => (a -> gr a b -> Node -> c) ->gr a b -> gr a b -> gr c b
nmapMod f g = gmap (\(p,v,l,s)->(p,v,f l g v,s))
        
attForce :: DynGraph gr => gr ((Double,Double),(Double,Double)) b -> Int -> Int -> Int -> (Double,Double)
attForce g n1 n2 typeOf = force
  where Just ((fx1,fy1),(x1,y1)) = lab g n1 
        Just ((fx2,fy2),(x2,y2)) = lab g n2
        dist = sqrt ((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
        d = if (dist/=0) then dist else 2
        effect = if(typeOf==1)then -1*(2*log d) else (1/sqrt d)
        force = (effect*(x1-x2)/d ,effect*(y1-y2)/d)
    
updateGraph :: DynGraph gr => gr ((Double,Double),(Double,Double)) b -> Int -> gr ((Double,Double),(Double,Double)) b
updateGraph inGraph 0 = inGraph
updateGraph inGraph n = outGraph
  where outFGraph = nmapMod funGraphMap inGraph inGraph
        outGraphloop = nmap updateLoc outFGraph
        outGraph = updateGraph outGraphloop (n-1)

updateLoc :: ((Double,Double),(Double,Double)) -> ((Double,Double),(Double,Double))
updateLoc l = newl
  where
            ((f1,f2),(x,y)) = l
            newl = ((f1,f2),(f1,f2))
            
repeatGraphUpdate :: DynGraph gr => gr a b -> Int -> gr ((Double,Double),(Double,Double)) b
repeatGraphUpdate g n = retGraph
  where fstGraph = nmap initGraph g
        retGraph = updateGraph fstGraph n

repeatUpdate update 0 = return update
repeatUpdate update n = 
  do 
    updated <- updateGraph update
    repeatUpdate updated (n-1)
        
giveList :: DynGraph gr => gr a b -> Int -> [(Double,Double)]
giveList graph n = list 
  where
    g = repeatGraphUpdate graph n
    list = map (\(Just (x,y))-> y) (map (lab g) (nodes g))


