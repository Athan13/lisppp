{
module Parser.Lexer (alexScanTokens, Token) where
}

%wrapper "posn"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  let					             { tok (\p s -> Let p) }
  if					             { tok (\p s -> If p) }
  do					             { tok (\p s -> Do p) }
  while					             { tok (\p s -> While p) }
  define					         { tok (\p s -> Define p) }
  read\-byte				         { tok (\p s -> Read p) }
  print\-byte					     { tok (\p s -> Write p) }
  $digit+				             { tok (\p s -> Int p (read s)) }
  [\+\-\*\/\%]                       { tok (\p s -> Op p (charToOp $ head s)) }
  [\=\<\>] | (\>\=) | (\<=) | (\!\=) { tok (\p s -> Comp p (strToComp s)) }
  $alpha [$alpha $digit \_ \']*		 { tok (\p s -> Var p s) }
  \(                                 { tok (\p s -> LPAREN p) }
  \)                                 { tok (\p s -> RPAREN p) }

{
tok f p s = f p s

data Op = Add | Sub | Mul | Div | Mod
    deriving (Show, Eq)
charToOp c = case c of {'+' -> Add; '-' -> Sub; '*' -> Mul; '/' -> Div; '%' -> Mod; _ -> undefined}

data Comp = Eq | Neq | Lt | Gt | Lte | Gte
    deriving (Show, Eq)
strToComp s = case s of {"=" -> Eq; "!=" -> Neq; "<" -> Lt; ">" -> Gt; "<=" -> Lte; ">=" -> Gte; _ -> undefined}

data Token =
    -- Keywords
    Let AlexPosn
    | If AlexPosn
    | Do AlexPosn
    | While AlexPosn
    | Define AlexPosn
    | Read AlexPosn
    | Write AlexPosn
    -- Symbols
    | Op AlexPosn Op
    | Comp AlexPosn Comp
    | Var AlexPosn String
    | Int AlexPosn Int
    | LPAREN AlexPosn
    | RPAREN AlexPosn
    deriving (Show, Eq)

token_posn :: Token -> AlexPosn
token_posn (Let p) = p
token_posn (If p) = p
token_posn (Do p) = p
token_posn (While p) = p
token_posn (Define p) = p
token_posn (Read p) = p
token_posn (Write p) = p

token_posn (Op p _) = p
token_posn (Comp p _) = p
token_posn (Var p _) = p
token_posn (Int p _) = p
token_posn (LPAREN p) = p
token_posn (RPAREN p) = p
}