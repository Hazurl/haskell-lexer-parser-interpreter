module Main where

import Lexer

main :: IO ()
main = do
    content <- readFile "example/hello_world.hsx"
    let ts = tokenize content
    --let ast = parseInstruction ts
    --let result = maybe "#error" interpret ast
    putStrLn ("\n### Content\n" ++ content ++ "\n### Interpretation\n" ++ (unlines (map show ts)))
    -- display (tokenize content)
