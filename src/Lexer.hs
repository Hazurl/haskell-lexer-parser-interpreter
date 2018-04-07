module Lexer (
    Token(..),

    tokenize
) where

import Data.Maybe
import Data.Maybe.HT
import Data.Functor
import Control.Lens

data Token =
    NewLine |
    Number Float |
    Ident String |
    Str String |
    Assign |
    Equal |
    Inferior |
    Superior |
    InfEqual |
    SupEqual |
    Different |
    Not |
    Plus |
    Minus |
    Multiplication |
    Division |
    Or |
    And |
    Print |
    Then | 
    Else | 
    If |
    For |
    End |
    Separator |
    LParenthesis |
    RParenthesis

instance Show Token where
    show NewLine = "NewLine"
    show (Number d) = "Decimal(" ++ show d ++ ")"
    show (Ident s) = "Ident(" ++ s ++ ")"
    show (Str s) = "Str(" ++ s ++ ")" 
    show Assign = "Assign" 
    show Equal = "Equal" 
    show Inferior = "Inferior" 
    show Superior = "Superior" 
    show InfEqual = "InfEqual" 
    show SupEqual = "SupEqual" 
    show Different = "Different"
    show Not = "Not"
    show Plus = "Plus"
    show Minus = "Minus"
    show Multiplication = "Multiplication"
    show Division = "Division"
    show Or = "Or"
    show And = "And"
    show Print = "Print"
    show Then = "Then"
    show Else = "Else"
    show If = "If"
    show For = "For"
    show End = "End"
    show Separator = "Separator"
    show LParenthesis = "LParenthesis"
    show RParenthesis = "RParenthesis"


--

eaters = [
    (eater "||" Or), 
    (eater "&&" And), 
    (eater "==" Equal), 
    (eater "<=" InfEqual), 
    (eater ">=" SupEqual), 
    (eater "!=" Different), 
    (eater "=" Assign), 
    (eater "<" Inferior), 
    (eater ">" Superior), 
    (eater "!" Not), 
    (eater "+" Plus), 
    (eater "-" Minus), 
    (eater "*" Multiplication), 
    (eater "/" Division), 
    (eater ";" Separator), 
    (eater "(" LParenthesis), 
    (eater ")" RParenthesis), 
    (eater "\n" NewLine), 
    eatString,
    eatNumber,
    eatIdent ]

tokenize :: String -> [Token]
tokenize = helper uselessChars eaters []

helper :: (String -> Int) -> [(String -> Maybe (Token, Int))] -> [Token] -> String -> [Token]
helper u fs ts s = 
    let us = drop (u s) s
    in case firstJust fs us of
        Nothing -> ts
        Just (t, n) -> helper u fs (ts ++ [t]) (drop n us)

firstJust :: [(a -> Maybe b)] -> a -> Maybe b
firstJust [] _ = Nothing
firstJust (f:fs) a = case f a of
    Nothing -> firstJust fs a
    Just r -> Just r 

uselessChars :: String -> Int 
uselessChars = length . takeWhile (`elem` " \t")

--


isAlpha :: Char -> Bool
isAlpha x = ('a' <= x) && ('z' >= x) || ('A' <= x) && ('Z' >= x) || x == '_'

isDigit :: Char -> Bool
isDigit x = ('0' <= x) && ('9' >= x)

isAlphaNum :: Char -> Bool
isAlphaNum x = isDigit x || isAlpha x

isSpace :: Char -> Bool
isSpace x = x `elem` " \t"

getKeyword :: String -> Token
getKeyword "then"  = Then
getKeyword "print" = Print
getKeyword "else"  = Else
getKeyword "if"    = If
getKeyword "for"   = For
getKeyword "end"   = End
getKeyword s       = Ident s

isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (p:ps) (s:ss) = p == s && isPrefix ps ss

--


eatUntil :: Char -> String -> Maybe String
eatUntil c s = 
    let r = takeWhile (/= c) s
        ok = (length s) /= (length r)
    in toMaybe ok r

eater :: String -> Token -> String -> Maybe (Token, Int)
eater r t s | isPrefix r s = Just (t, length r)
eater _ _ _ = Nothing

eatNumber :: String -> Maybe (Token, Int)
eatNumber ss = case (reads ss :: [(Float, String)]) of
    [(n, s)] -> Just ((Number n), (length ss) - (length s))
    _ -> Nothing

eatString :: String -> Maybe (Token, Int)
eatString ('"':s) = 
    let mrs = (eatUntil '"' s) <&> (++ "\"") <&> ('"' :)
    in mrs >>= (\rs -> Just (Str rs, length rs))
eatString _ = Nothing

eatIdent :: String -> Maybe (Token, Int)
eatIdent (s:ss) | isAlpha s = 
    let i = s:(takeWhile isAlphaNum ss)
    in Just $ (getKeyword i, length i) 
eatIdent _ = Nothing