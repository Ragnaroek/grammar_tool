module Grammar.LL (
	isSLL,
	LLAnswer,
	isProductionLL,
	Answer(..),
) where

import Grammar.Grammar
import Grammar.GFA

import Util
import Data.List
import Debug.Trace

data Answer a = Yes |
                No { why :: a }
    deriving (Show, Eq)

type LLAnswer = [(Production, Production, Language)]

isSLL :: Int -> Grammar -> Result -> Result -> Answer LLAnswer
isSLL k g first follow = if' (null violations) Yes (No $ concat $ map why violations)
    where violations = filter (/= Yes) $ map (isProductionLL (fifo first follow k)) (map (prodsWithHead g) (nonTerminals g))
	
isProductionLL :: (Production -> Language) -> [Production] -> Answer LLAnswer
isProductionLL fifo allAlternatives = allDisjunct fifo allPairs 
    where len = length allAlternatives
          allPairs = [(allAlternatives !! i, allAlternatives !! j) | i <- [0..(len-1)], j <- [0..(len-1)], j > i]

--allYes :: (Eq a) => [Answer a] -> Answer a
--allYes [] = Yes
--allYes (x:xs) | x == Yes = allYes xs
--              | otherwise = x
              
allDisjunct :: (Production -> Language) -> [(Production, Production)] -> Answer LLAnswer
allDisjunct fifo l = case result of
                        [] -> Yes
                        _ -> No result
    where intersection p1 p2 = (fifo p1) `intersect` (fifo p2)
          result = filter (not . null . thrd3) $ map (\(p1,p2) -> (p1, p2, (intersection p1 p2))) l

thrd3 :: (a, b, c) -> c
thrd3 (_, _, c) = c