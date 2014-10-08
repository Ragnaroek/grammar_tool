module Grammar.Grammar (
    ProductionElement(..),
    Production,
    mkProduction,
    prodHead,
    prodBody,
    Grammar,
    mkGrammar,
    terminals,
    nonTerminals,
    productions,
    startSymbol,
    prodTerminals,
    prodNonTerminals,
    prodsWithHead,
    epsilon,
    eofSym,
    inBody,
    Word,
    Language,
    isBodyEpsilon,
) where

import Data.List

epsilon = Terminal "ɛ"
eofSym = Terminal "#"

type Word = [ProductionElement]
type Language = [Word]

data ProductionElement = Terminal {
    terminalName :: String
} | NonTerminal {
    nTerminalName :: String   
} deriving (Ord, Eq)

instance Show ProductionElement where
    show (Terminal n) = n
    show (NonTerminal n) = n

isTerminal :: ProductionElement -> Bool
isTerminal (Terminal _) = True
isTerminal _ = False

isNonTerminal :: ProductionElement -> Bool
isNonTerminal (NonTerminal _) = True
isNonTerminal _ = False

data Production = Production {
    prodHead :: ProductionElement,
    prodBody :: [ProductionElement] 
} deriving (Ord, Eq)

instance Show Production where
    show (Production h b) = (show h) ++ " -> " ++ (show b)

mkProduction :: ProductionElement -> [ProductionElement] -> Production
mkProduction h@(NonTerminal _) body = Production h body
mkProduction _ _ = (error "head of production must be non-terminal")

prodTerminals :: Production -> [ProductionElement]
prodTerminals prod = filter isTerminal (prodBody prod)

prodNonTerminals :: Production -> [ProductionElement]
prodNonTerminals prod = (prodHead prod) : filter isNonTerminal (prodBody prod)

inBody :: Production -> ProductionElement -> Bool
inBody prod e = elem e (prodBody prod)

isBodyEpsilon :: Production -> Bool
isBodyEpsilon prod = (length (prodBody prod)) == 1 && (prodBody prod) !! 0 == epsilon

data Grammar = Grammar {
    terminals :: [ProductionElement], -- a set, but list is used for comprehension
    nonTerminals :: [ProductionElement],
    productions :: [Production],
    startSymbol :: ProductionElement
}

instance Show Grammar where
    show (Grammar sigma n p s) = "Grammar \nSigma=" ++ show sigma ++ "\nN=" ++ show n ++ "\nS=" ++ show s ++
          "\nP=" ++
          foldl' (\accu prod -> accu ++ "\n" ++ (show prod)) "" p  

mkGrammar :: [ProductionElement] -> [ProductionElement] -> [Production] -> ProductionElement -> Grammar
mkGrammar ts nts ps s | (all isTerminal ts) && (all isNonTerminal nts) && (isNonTerminal s) = Grammar ts nts ps s
                      | otherwise = (error "invalid grammar")
                      
prodsWithHead :: Grammar -> ProductionElement -> [Production]
prodsWithHead g n = filter ((== n) . prodHead) (productions g)


--allSet :: (a -> Bool) -> S.Set a -> Bool                      
--allSet p = S.fold (\a b -> ((p a) && b)) True
