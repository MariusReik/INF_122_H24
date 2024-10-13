module Oblig1 where

import Data.Char (isDigit, isAlpha, isSpace)

-- Split the input into tokens
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

-- Parse a term (number or variable)
parseTerm :: [String] -> (Ast, [String])
parseTerm (t:ts)
  | all isDigit t = (Tall (read t), ts)
  | all isAlpha t = (Var t, ts)
  | t == "(" = let (expr, rest) = parseExpr ts in (expr, tail rest)
parseTerm _ = (Tall 0, [])

-- Parse factors (multiplication/division)
parseFactor :: [String] -> (Ast, [String])
parseFactor = parseBinaryOp parseTerm ["*", "/"] BinOp

-- Parse expressions (addition/subtraction)
parseExpr :: [String] -> (Ast, [String])
parseExpr = parseBinaryOp parseFactor ["+", "-"] BinOp

-- General binary operation parser
parseBinaryOp :: ([String] -> (Ast, [String])) -> [String] -> (Op -> Ast -> Ast -> Ast) -> [String] -> (Ast, [String])
parseBinaryOp parse lowerPrecedence opSymbols tokens =
  let (left, rest) = parse tokens
  in case rest of
       (op:ts) | op `elem` lowerPrecedence ->
           let (right, rest') = parseBinaryOp parse lowerPrecedence opSymbols ts
           in (BinOp (toOp op) left right, rest')
       _ -> (left, rest)

-- Convert string to operation
toOp :: String -> Op
toOp "+" = Add
toOp "-" = Sub
toOp "*" = Mult
toOp "/" = Div

-- Main parse function
parse :: String -> Ast
parse s = fst (parseExpr (tokenise s))

-- Evaluate AST
eval :: Ast -> Int
eval (Tall n) = n
eval (BinOp op left right) = applyOp op (eval left) (eval right)

-- Apply operator
applyOp :: Op -> Int -> Int -> Int
applyOp Add  = (+)
applyOp Sub  = (-)
applyOp Mult = (*)
applyOp Div  = div

-- Pretty print in infix
ppInfix :: Ast -> String
ppInfix (Tall n) = show n
ppInfix (Var v) = v
ppInfix (BinOp op left right) =
    "(" ++ ppInfix left ++ " " ++ opStr op ++ " " ++ ppInfix right ++ ")"


-- Pretty print in prefix
ppPN :: Ast -> String
ppPN (Tall n) = show n
ppPN (Var v) = v
ppPN (BinOp op left right) = opStr op ++ " " ++ ppPN left ++ " " ++ ppPN right

-- Pretty print in postfix
ppOPN :: Ast -> String
ppOPN (Tall n) = show n
ppOPN (Var v) = v
ppOPN (BinOp op left right) = ppOPN left ++ " " ++ ppOPN right ++ " " ++ opStr op

-- Convert operation to string
opStr :: Op -> String
opStr Add  = "+"
opStr Sub  = "-"
opStr Mult = "*"
opStr Div  = "/"

-- Find variable value in environment
findVar :: [(String, Int)] -> String -> Int
findVar env var = maybe 0 id (lookup var env)

-- Evaluate variable
evalVar :: Ast -> [(String, Int)] -> Int
evalVar (Tall n) _ = n
evalVar (Var v) env = findVar env v
evalVar (BinOp op left right) env = applyOp op (evalVar left env) (evalVar right env)