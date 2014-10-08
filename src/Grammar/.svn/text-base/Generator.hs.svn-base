module Grammar.Generator (
    genCParser,
) where

import Grammar.Grammar
import Grammar.GFA
import Grammar.LL

import Data.List
import Debug.Trace
import Util
import qualified Data.Map as M

-- TODO make specialised TextGen Monad 

-- The generator assumes a LL(2) parser is generated!

genDebug = True;
genDecls = False;
genParseFunc = False;

genCParser :: Grammar -> (Production -> Language) -> String
genCParser g fifo = let (decls, code) = genProductions g fifo in
    (if' genDecls decls "") ++ "\n\n" ++
    (if' genParseFunc (genParserFunc g) "") ++ "\n\n" ++
    code

genParserFunc :: Grammar -> String
genParserFunc g = 
    "void Parser::parse() {\n" ++
    "\t/* init lookaheads*/\n" ++
    "\tinit();\n" ++
    "\t" ++ (mkFuncCall (startSymbol g)) ++ "\n" ++
    "\tmatch(" ++ (litToTag "#") ++ ");\n" ++
    "}\n"
    
genProductions :: Grammar -> (Production -> Language) -> (String, String)
genProductions g fifo = (decls, code)
    where decls = (unlines $ map (\n -> "void " ++ (mkFuncCall n)) (nonTerminals g)) ++ "//test functions\n" ++ helperDecls
          (helperDecls, code) = (progr g fifo)

mkFuncCall :: ProductionElement -> String
mkFuncCall e = (mkFuncName e) ++ "();";

mkFuncName :: ProductionElement -> String
mkFuncName (NonTerminal n) = (filter (\c ->  (c /= '<') && (c /= '>')) n)
mkFuncName _ = ("error can make func name only for nonterminals")

progr :: Grammar -> (Production -> Language) -> (String, String)
progr g fifo = ((unlines $ filter (/= "") $ map fst prodFuncs),
                (unlines $ map snd prodFuncs))
    where prodFuncs = map (mkProdFunc g fifo) (nonTerminals g)

mkProdFunc :: Grammar -> (Production -> Language) -> ProductionElement -> (String, String)
mkProdFunc g fifo n = (decls,
    helperFuncs ++ "\n" ++ 
    "void Parser::" ++ (mkFuncName n) ++ "(){\n" ++
    if' genDebug ("cerr << \"" ++ (mkFuncName n) ++ "\" << endl;\n") "" ++
    body ++
    if' genDebug ("cerr << \"\\t<<" ++ (mkFuncName n) ++ "\"<< endl;\n") "" ++
    "}\n")
    where (decls, helperFuncs, body) = mkProdFuncBody g fifo n

mkProdFuncBody :: Grammar -> (Production -> Language) -> ProductionElement -> (String, String, String)
mkProdFuncBody g fifo n | (length alternatives) == 1 = ("", "", codeForRhs (alternatives !! 0))
                        | otherwise = codeForChoice g fifo alternatives
    where alternatives = prodsWithHead g n

codeForChoice :: Grammar -> (Production -> Language) -> [Production] -> (String, String, String)
codeForChoice g fifo prods = (testFuncDecls, testFuncDefs, body)
    where body = unlines $ intersperse " else " $ filter (/= "") $ 
    					((map codeForTestAndApply nonEpsProdTestFuncMap) ++ [(codeForFail prods)])
    	  testFuncDefs = unlines $ map (codeForTestFunc fifo onlyLook1) nonEpsProdTestFuncMap
    	  nonEpsProds = (nonEpsilonProds prods)
          nonEpsProdTestFuncMap = map (\(prod,i) -> (prod, "test" ++ mkFuncName (prodHead prod) ++ show i)) (zip nonEpsProds [0..(length nonEpsProds)])
          onlyLook1 = lookahead1Enough fifo prods
          testFuncDecls = unlines $ map (\(p,n) -> "bool " ++ n ++ "();") nonEpsProdTestFuncMap

codeForTestFunc :: (Production -> Language) -> Bool -> (Production, String) -> String
codeForTestFunc fifo l1Enough (prod, testFuncName) =
	"// test for alternative: " ++ (show prod) ++ "\n" ++
	"bool Parser::" ++ testFuncName ++ "() {\n" ++
	if' l1Enough (codeForLook1Test (fifo prod)) (codeForLook1And2 (fifo prod)) ++
	"}\n" 

-- TODO: helperDecls in codeForChoice rausgeben

codeForLook1Test :: Language -> String
codeForLook1Test l = "return (" ++ (orL1 l) ++ ");"
	where orL1 = unwords . (intersperse "||") . (map codeForTestWord) . reduceTo1Prefix 

codeForLook1And2 :: Language -> String
codeForLook1And2 l = (unlines $ intersperse " else " $ map codeForPrefixEntry $ M.toList prefixMap) ++ "\n return false;"
	where prefixMap = foldl' putInMap M.empty l
	      putInMap m w = M.alter (addToPrefixMapValue w) (kPrefix 1 w) m  

addToPrefixMapValue :: Word -> Maybe Language -> Maybe Language
addToPrefixMapValue w Nothing = Just [drop 1 w]
addToPrefixMapValue w (Just l) = Just $ (drop 1 w) : l 

codeForPrefixEntry :: (Word, Language) -> String
codeForPrefixEntry (w, l) = 
	"if(" ++ codeForTestWord w ++ ") {\n" ++
	(codeForSwitchLookahead2 $ nub l) ++
    "}"
    
codeForSwitchLookahead2 :: Language -> String
codeForSwitchLookahead2 l | [] `elem` l = trace "Warning: empty lookahead2 found" ""
                          | otherwise = switchCode
       where switchCode = "switch(lookahead2){\n" 
                          ++ (unlines $ map (\w -> "case " ++ (codeForToken $ head w) ++ ":") $ l) 
                          ++ "return true;\n"
                          ++ "default: return false;\n}" 
    
codeForTestAndApply :: (Production, String) -> String
codeForTestAndApply (prod, testFuncName) | (isBodyEpsilon prod) = ""
       | otherwise = "if(" ++ testFuncName ++ "()){\n\t" ++
       	                 codeForRhs prod ++
       	             "\n}"
									  	 
nonEpsilonProds :: [Production] -> [Production]
nonEpsilonProds prods = filter (not . isBodyEpsilon) prods


lookahead1Enough :: (Production -> Language) -> [Production] -> Bool
lookahead1Enough fifoOld alts | isLLWithLookahead1 == Yes = trace ("could optimize [lookahead1 enough] on: " ++ show (prodHead $ head alts)) True
                              | otherwise = False 
    where fifo1 = (mkFifo1 fifoOld)
          isLLWithLookahead1 = isProductionLL fifo1 alts


mkFifo1 :: (Production -> Language) -> (Production -> Language)
mkFifo1 fifo_n = (\prod -> nub $ reduceTo1Prefix (fifo_n prod)) 

reduceTo1Prefix :: Language -> Language
reduceTo1Prefix l = nub $ map (kPrefix 1) l


codeForFail :: [Production] -> String
codeForFail prods | any isBodyEpsilon prods = ""
                  | otherwise = "{ syntax_error(); }"

codeForTestWord :: Word -> String
codeForTestWord [] = error "Generating empty test"
codeForTestWord (t1:[]) = "(lookahead1 == " ++ (codeForToken t1) ++ ")"
codeForTestWord (t1:t2:[]) = "(" ++ "lookahead1 == " ++ (codeForToken t1) 
                              ++ " && lookahead2 == " ++ (codeForToken t2) ++ ")"                          

codeForToken :: ProductionElement -> String
codeForToken (Terminal t) = (litToTag t)
codeForToken _ = error "codeForToken only for terminals"

codeForRhs :: Production -> String
codeForRhs prod = unwords $ map codeForPElem (prodBody prod)

codeForPElem :: ProductionElement -> String
codeForPElem n@(NonTerminal _) = mkFuncCall n 
codeForPElem (Terminal t) = "match("++ (litToTag t) ++ ");"

litToTag :: String -> String
litToTag l = "Token::" ++ (tag l)

tag :: String -> String
tag "#" = "T_EOF"
tag "(" = "T_LPAREN"
tag ")" = "T_RPAREN"
tag "{" = "T_LCURLYPAREN"
tag "}" = "T_RCURLYPAREN"
tag "[" = "T_LSQPAREN"
tag "]" = "T_RSQPAREN"
tag ";" = "T_SEMICOLON"
tag "," = "T_COMMA"
tag "." = "T_POINT"
tag "||" = "T_OR"
tag "&&" = "T_AND"
tag "!" = "T_NOT"
tag "==" = "T_EQ"
tag "!=" = "T_NE"
tag "?" = "T_LT" -- ? Kodierung fuer <
tag "?=" = "T_LE"
tag "??" = "T_GT"
tag "??=" = "T_GE"
tag "=" = "T_ASSIG"
tag "+" = "T_PLUS"
tag "-" = "T_MINUS"
tag "*" = "T_TIMES"
tag "/" = "T_DIV"
tag "%" = "T_MOD"
tag "IDENT" = "T_IDENT"
tag "INTEGER_LITERAL" = "T_NUMBER"
tag "class" = "T_CLASS"
tag "public" = "T_PUBLIC"
tag "static" = "T_STATIC"
tag "void" = "T_VOID"
tag "int" = "T_INT"
tag "boolean" = "T_BOOLEAN"
tag "while" = "T_WHILE"
tag "if" = "T_IF"
tag "else" = "T_ELSE"
tag "return" = "T_RETURN"
tag "null" = "T_NULL"
tag "false" = "T_FALSE"
tag "true" = "T_TRUE"
tag "this" = "T_THIS"
tag "new" = "T_NEW"
tag "extends" = "T_EXTENDS"		-- change by barbara
tag x = error ("unknown tag=" ++ x)






--optimizeUseOnlyLookahead2 :: (Production -> Language) -> [Production] -> Bool
--optimizeUseOnlyLookahead2 fifo alts | (allEq1Prefix $ concat $ map fifo altsWithoutEpsilon) == True 
--                                           = trace ("could optimize [only lookahead2] on: " ++ show (prodHead $ head alts)) True
--                                    | otherwise = False
--          where altsWithoutEpsilon = filter (not . isBodyEpsilon) alts


--codeForTestAndApplyAlternative :: Bool -> Grammar -> (Production -> Language) -> Production -> String
--codeForTestAndApplyAlternative b g fifo prod | (isBodyEpsilon prod) = ""
--    | otherwise =  
--        "if(" ++ (codeForTestLang b lookaheads) ++ ") {\n\t" ++
--            codeForRhs prod ++
--        "\n}"
--            where lookaheads = fifo prod
            
--allEq1Prefix :: Language -> Bool
--allEq1Prefix [] = error "empty language!"
--allEq1Prefix l = all (\w -> (kPrefix 1 w) == (kPrefix 1 (head l))) l

-- 1st arg = only lookahead 1


-- 1st arg = only lookahead 1


