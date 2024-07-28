module Main (main) where

import qualified Naloge.Naloga0 as N0
import qualified Naloge.Naloga1 as N1
import qualified Naloge.Naloga2 as N2
import qualified Naloge.Naloga3 as N3
import qualified Naloge.Naloga4 as N4
import qualified Naloge.Naloga5 as N5

import System.Environment (getArgs)
import Data.Maybe (listToMaybe, isJust)
import Numeric (readDec)

simples :: [String -> String]
simples = [N0.simple, N1.simple, N2.simple, N3.simple, N4.simple, N5.simple]

main :: IO ()
main = do
    args <- getArgs
    -- check if there is at least one argument and if 1st argument is valid
    if isJust (listToMaybe args) &&
       let firstIntRead = readDec . head $ args in
       (not . null $ firstIntRead) &&
       (fst . head $ firstIntRead) `elem` [0..5]
        --fromMaybe False $ (notElem . read <$> listToMaybe args) <*> Just [0..5]
        then do
            cont <- getContents
            putStrLn (simples !! read (head args) $ cont)
        else do
            putStrLn "1st argument must be a number from 0 to 5, corresponding to the task"
