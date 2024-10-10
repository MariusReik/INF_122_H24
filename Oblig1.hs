module Oblig1 where

import Data.Char (isDigit, isAlpha, isSpace)

tokenise :: String -> [String]
tokenise [] = []
tokenise (c:cs)
  | isDigit c = let (digits, rest) = span isDigit (c:cs) in digits : tokenise rest
  | isAlpha c = let (letters, rest) = span isAlpha (c:cs) in letters : tokenise rest
  | c `elem` "+-*/()" = [c] : tokenise cs
  | isSpace c = tokenise cs
  | otherwise = tokenise cs

data Op = Add | Sub | Mult | Div deriving (Show, Eq)
data Ast = BinOp Op Ast Ast | Tall Int | Var String deriving (Show, Eq)

parseTerm :: [String] -> (Ast, [String])
parseTerm (t:ts)
  | all isDigit t = (Tall (read t), ts)
  | all isAlpha t = (Var t, ts)
  | t == "(" = let (expr, rest) = parseExpr ts in (expr, tail rest)
parseTerm _ = (Tall 0, [])

parseFactor :: [String] -> (Ast, [String])
parseFactor = parseBinOp parseTerm ["*", "/"] BinOp

parseExpr :: [String] -> (Ast, [String])
parseExpr = parseBinOp parseFactor ["+", "-"] BinOp

parseBinOp :: ([String] -> (Ast, [String])) -> [String] -> (Op -> Ast -> Ast -> Ast) -> [String] -> (Ast, [String])
parseBinOp parse lowerPrecedence opSymbols tokens =
  let (left, rest) = parse tokens
  in case rest of
       (op:ts) | op `elem` lowerPrecedence ->
           let (right, rest') = parseBinOp parse lowerPrecedence opSymbols ts
           in (BinOp (toOp op) left right, rest')
       _ -> (left, rest)

toOp :: String -> Op
toOp "+" = Add
toOp "-" = Sub
toOp "*" = Mult
toOp "/" = Div

parse :: String -> Ast
parse s = fst (parseExpr (tokenise s))

eval :: Ast -> Int
eval (Tall n) = n
eval (BinOp op l r) = applyOp op (eval l) (eval r)

applyOp :: Op -> Int -> Int -> Int
applyOp Add  = (+)
applyOp Sub  = (-)
applyOp Mult = (*)
applyOp Div  = div

ppInfix :: Ast -> String
ppInfix (Tall n) = show n
ppInfix (Var v) = v
ppInfix (BinOp op l r) = "(" ++ ppInfix l ++ " " ++ opStr op ++ " " ++ ppInfix r ++ ")"

ppPN :: Ast -> String
ppPN (Tall n) = show n
ppPN (Var v) = v
ppPN (BinOp op l r) = opStr op ++ " " ++ ppPN l ++ " " ++ ppPN r

ppOPN :: Ast -> String
ppOPN (Tall n) = show n
ppOPN (Var v) = v
ppOPN (BinOp op l r) = ppOPN l ++ " " ++ ppOPN r ++ " " ++ opStr op

opStr :: Op -> String
opStr Add  = "+"
opStr Sub  = "-"
opStr Mult = "*"
opStr Div  = "/"

findVar :: [(String, Int)] -> String -> Int
findVar env var = maybe 0 id (lookup var env)

evalVar :: Ast -> [(String, Int)] -> Int
evalVar (Tall n) _ = n
evalVar (Var v) env = findVar env v
evalVar (BinOp op l r) env = applyOp op (evalVar l env) (evalVar r env)