module Main 
where

import Grammar.Parser
import Grammar.GFA
import Grammar.Grammar
import Grammar.LL
import Grammar.Generator
import qualified Data.Map as M
import qualified Data.Set as S


grammarFile = "/Users/mb/Dropbox/Projekte/gfc/grammar/grammar.gf"
ll_n = 1 

main :: IO ()
main = do
    input <- readFile grammarFile
    let grammar = case (parseGF input) of
                    Left e -> (error (show e))
                    Right g -> g
    let fi = first ll_n grammar
    let fo = follow ll_n fi grammar
    let check = isSLL ll_n grammar fi fo
    if hasConflicts check
       then print $ "Grammar is not LL(" ++ (show ll_n) ++ "), see conflicts.txt for more"
       else print $ "Grammar is LL(" ++ (show ll_n) ++ ")"
    writeFile "./conflicts.txt" $ conflicts check
    --let code = genCParser grammar (fifo fi fo ll_n)
    --writeFile "./testcode.c" code


hasConflicts :: Answer LLAnswer -> Bool
hasConflicts (No _) = True
hasConflicts _ = False

conflicts :: Answer LLAnswer -> String
conflicts y@Yes = show y
conflicts (No l) = unlines $ map (\(p1,p2,_) -> show p1 ++ "<-/->" ++ show p2) l
