module Grammar.Parser (
    parseGF,
)
where

import Grammar.Grammar
import Text.ParserCombinators.Parsec
import Data.List 

parseGF :: String -> Either ParseError Grammar
parseGF input = do 
    prods <- parse grammar "(unknown)" input
    let s = (prodHead (head prods))
    let nts = foldl' (\accu prod -> (union accu (prodNonTerminals prod))) [s] prods
    let ts = foldl' (\accu prod -> (union accu (prodTerminals prod))) [] prods
    return $ mkGrammar ts nts prods s 

grammar :: CharParser () ([Production])
grammar = do 
    prods <- endBy1 production eol
    eof
    return prods

production = do
    h <- element
    blanks
    string "->"
    blanks
    body <- sepBy element blanks
    return $ mkProduction h body

element = try nonterminal <|> terminal

nonterminal = do
    char '<'
    text <- many1 $ noneOf ">"
    char '>'
    return $ NonTerminal ("<" ++ text ++ ">")

terminal = do
    text <- many1 $ noneOf whitespaces
    return $ Terminal text

blanks = many1 (char ' ')

eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    
whitespaces = " \t\n\r\f\v\xa0"