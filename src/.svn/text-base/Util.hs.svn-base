
{- A set of useful functions that don't deserve a new package -}

module Util (
    traceValue,
    uQName,
    subsetOf,
    if'
) where

import Debug.Trace
import qualified Text.XML.Light as Xml
import Data.List

{- Tracing -}

traceValue :: Show a => a -> a
traceValue exp = trace (show exp) exp


{- Xml -}
	
uQName :: String -> Xml.QName
uQName s = 	(Xml.QName s Nothing Nothing)


{- Lists -}

subsetOf :: Eq a => [a] -> [a] -> Bool
subsetOf x universe = x `intersect` universe == x


{- Stuff -}

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
