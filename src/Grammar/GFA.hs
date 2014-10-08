
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, PackageImports #-}

module Grammar.GFA (
   first,
   follow,
   fifo,
   Result,
   lookupResult,
   KConcatenation,
   kConcat,
   kPrefix,
   firstOfProdBody,
) where

import Grammar.Grammar
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import "mtl" Control.Monad.State

import Debug.Trace

type Result = M.Map ProductionElement Language

first :: Int -> Grammar -> Result
first k g = execState (fixpoint g (transferFirstk k)) $ M.fromList [(n,[]) | n <- (nonTerminals g)]

follow :: Int -> Result -> Grammar -> Result
follow k firstResult g = execState (fixpoint g (transferFollowk k firstResult)) 
							$ M.fromList $ ((startSymbol g),[[eofSym]]) : [(n,[]) | n <- (nonTerminals g), n /= (startSymbol g)]


firstOfProdBody :: Int -> Production -> Result -> Language 
firstOfProdBody k prod first = firstOfPElems k (prodBody prod) first

firstOfPElems :: Int -> [ProductionElement] -> Result -> Language
firstOfPElems k es first = foldFirsts k (map (lookupResult first) es)

-- a special fold
foldFirsts :: Int -> [Language] -> Language
foldFirsts _ [] = (error "foldFirst cannot be applied to empty list")
foldFirsts _ (x:[]) = x
foldFirsts k (x:xs) = ((kConcat k) x (foldFirsts k xs))

fifo :: Result -> Result -> Int -> Production -> Language
fifo first follow k prod = ((kConcat k) (firstOfProdBody k prod first) (lookupResult follow (prodHead prod))) 

wConcat :: Word -> Word -> Word
wConcat [Terminal "ɛ"] v = v
wConcat u [Terminal "ɛ"] = u
wConcat u v = u ++ v

kPrefix :: Int -> Word -> Word
kPrefix = take

class KConcatenation a where
    kConcat :: Int -> a -> a -> a

instance KConcatenation Word where
    kConcat k u v = kPrefix k $ wConcat u v

instance KConcatenation Language where
    kConcat k l1 l2 = nub $ [ x `conc` y | x <- l1, y <- l2 ]
        where conc = (kConcat k)          

fixpoint :: Grammar -> (Grammar -> ProductionElement -> Result -> Language) -> State (Result) ()
fixpoint g trans = do
    old <- get
    mapM_ (\n -> (modify (\s -> M.insert n (trans g n s) s))) (nonTerminals g)
    new <- get
    unless (old == new) (fixpoint g trans)

-- transfer function for first_k          
transferFirstk :: Int -> Grammar -> ProductionElement -> Result -> Language
transferFirstk k g n r = nub $ concat [ nub $ firstRhs prod | prod <- alternatives] 
        where alternatives = prodsWithHead g n
              firstRhs prod =  firstOfProdBody k prod r  --foldl1' (kConcat k) (firstsProd prod)
              firstsProd prod = map (lookupResult r) (prodBody prod)


transferFollowk :: Int -> Result -> Grammar -> ProductionElement -> Result -> Language
transferFollowk k firstResult g n r | n == (startSymbol g) = [[eofSym]]
                                    | otherwise = followNonStartSym k firstResult g n r

followNonStartSym :: Int -> Result -> Grammar -> ProductionElement -> Result -> Language
followNonStartSym k firstResult g n r = nub $ concat (nub $ map (combinePair k g firstResult r) (followPairs g n))

combinePair :: Int -> Grammar -> Result -> Result -> (ProductionElement, Maybe [ProductionElement]) -> Language
combinePair _ g _ followResult (n, Nothing) = lookupResult followResult n
combinePair k g firstResult followResult (n, (Just beta)) = (firstOfPElems k beta firstResult) --(lookupResult firstResult beta) 
 														    `conc` 
 														    (lookupResult followResult n)
 													      where conc = (kConcat k)			  

followPairs :: Grammar -> ProductionElement -> [(ProductionElement, Maybe [ProductionElement])]
followPairs g n = [((prodHead prod), (neighbourOf n prod)) | prod <- (productions g), (inBody prod n)] 

neighbourOf :: ProductionElement -> Production -> Maybe [ProductionElement]
neighbourOf n prod | ((length indices) /= 1) = (error "unsupported grammar (NonTerminal occures more than once in rhs of production)") 
                   | otherwise = maybeNeighbour ((head indices) + 1) prod
               where indices = (elemIndices n (prodBody prod))

maybeNeighbour :: Int -> Production -> Maybe [ProductionElement]
maybeNeighbour i prod | i >= (length (prodBody prod)) = Nothing
                      | otherwise = Just $ (drop i (prodBody prod))  --(prodBody prod) !! i

-- return result for ProductionElement in current Result              
lookupResult :: Result -> ProductionElement -> Language
lookupResult r t@(Terminal _) = [[t]]
lookupResult r n@(NonTerminal _) = fromJust $ M.lookup n r
