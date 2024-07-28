module Naloge.Naloga4 (simple, astar) where

import Data.Bifunctor (bimap)
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.PQueue.Prio.Min as Q
import Data.Maybe (isJust, maybe)
import Data.Either (isRight, isLeft)

-- default lookup value
bajillion :: Int
bajillion = maxBound - 10

uncurry4 :: (t1 -> t2 -> t3 -> t4 -> t5) -> (t1, t2, t3, t4) -> t5
uncurry4 f (a,b,c,d) = f a b c d

type Node = (Int, Int)
type CurrentState = (Q.MinPQueue Int Node, HashMap Node Int, HashMap Node Int, HashMap Node Node)

astar :: Node -> Node -> [Node] -> (Node -> Node -> Int) -> Node -> {-[Either CurrentState (Maybe [Node])]-} Maybe [Node]
astar start end obstacles distFunc bounds = (\(Right x) -> x) . head . dropWhile isLeft . iterate (\(Left x) -> uncurry4 (astarStepped start end obstacles distFunc bounds) x) $ Left (Q.singleton (distFunc start end) start, Map.singleton start 0, Map.singleton start (distFunc start end), Map.empty)

-- steps will be used for tui animation soon:tm:
astarStepped :: Node -> Node -> [Node] -> (Node -> Node -> Int) -> Node -> Q.MinPQueue Int Node -> HashMap Node Int -> HashMap Node Int -> HashMap Node Node -> Either CurrentState (Maybe [Node])
astarStepped start end obstacles distFunc (boundX,boundY) = astar_
    where
        astar_ :: Q.MinPQueue Int Node -> HashMap Node Int -> HashMap Node Int -> HashMap Node Node -> Either CurrentState (Maybe [Node])
        astar_ openSet fromStart distGuess cameFrom
            | Q.null openSet = Right Nothing
            | curr == end    = Right $ assemblePath cameFrom curr
            | otherwise      = Left $ (,,,) openSet' fromStart' distGuess' cameFrom'
            where curr = snd (Q.findMin openSet)
                  inBounds x y = 0 <= x && 0 <= y && x <= boundX && y <= boundY
                  inBoundMove :: Node -> Int -> Int -> Bool
                  inBoundMove (nx, ny) x y = inBounds (nx+x) (ny+y)
                  openSet' = foldr (\(n,score) queue -> Q.insert (score+distFunc n end) n queue) (Q.deleteMin openSet) $ filter ((`notElem` Q.elemsU openSet) . fst) bestNeighbors
                  neighbors = let neighborCells = [bimap (+x) (+y) curr | x <- [-1,0,1], y <- [-1,0,1], (x==0) /= (y==0), inBoundMove curr x y] in filter (`notElem` obstacles) neighborCells
                  bestNeighbors = filter (\(n, score) -> score < Map.lookupDefault bajillion n fromStart) $ map (\n -> (n, Map.lookupDefault bajillion curr fromStart+1)) neighbors
                  fromStart' = foldr (\(n,score) m -> Map.insert n score m) fromStart bestNeighbors
                  distGuess' = foldr (\(n,score) m -> Map.insert n (score+distFunc n end) m) distGuess bestNeighbors
                  cameFrom' = foldr (\(n,_) m -> Map.insert n curr m) cameFrom bestNeighbors
                  assemblePath cf n = fst . head . dropWhile (isJust . snd) $ iterate itFun (Nothing, Just [n])
                      where itFun (_, c@(Just (h:_))) = (c, (:) <$> Map.lookup h cf <*> c)
                            itFun (_, Just []) = (Just [], Nothing)
                            itFun (prev, Nothing) = (prev, Nothing)

simple :: String -> String
simple = patternMatchAndDo . map (map read . words) . lines
    where
        patternMatchAndDo ([boundX,boundY]
                          :[ux,uy,rx,ry]
                          :_
                          :zasidrane) = maybe "invalid input" (show . (\x -> x-2) . length) $ astar (rx,ry) (ux,uy) (generateObstacles zasidrane) df (boundX, boundY)
        generateObstacles :: [[Int]] -> [Node]
        generateObstacles = concatMap (\[x1, y1, x2, y2] -> (,) <$> [x1..x2] <*> [y1..y2])
        df (x1,y1) (x2,y2) = abs x1-x2 + abs y1-y2
