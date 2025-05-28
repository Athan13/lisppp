{
module Parser.Lexer where
}

%wrapper "monad"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  let					               { tok (\s -> Let) }
  if					               { tok (\s -> If) }
  do					               { tok (\s -> Do) }
  define			  		         { tok (\s -> Define) }
  read\-byte				         { tok (\s -> Read) }
  print       					     { tok (\s -> Write) }
  $digit+				             { tok (\s -> Num (read s)) }
  [\+\-\*\/\%]                       { tok (\s -> Op (charToOp $ head s)) }
  [\=\<\>] | (\>\=) | (\<=) | (\!\=) { tok (\s -> Comp (strToComp s)) }
  $alpha [$alpha $digit \_ \']*		 { tok (\s -> Var s) }
  \(                                 { tok (\s -> LPAREN) }
  \)                                 { tok (\s -> RPAREN) }

{
-- Tokens
data Op = Add | Sub | Mul | Div | Mod
    deriving (Show, Eq)
charToOp c = case c of {'+' -> Add; '-' -> Sub; '*' -> Mul; '/' -> Div; '%' -> Mod; _ -> undefined}

data Comp = Eq | Neq | Lt | Gt | Lte | Gte
    deriving (Show, Eq)
strToComp s = case s of {"=" -> Eq; "!=" -> Neq; "<" -> Lt; ">" -> Gt; "<=" -> Lte; ">=" -> Gte; _ -> undefined}

data Token =
    -- Keywords
    Let
    | If
    | Do
    | Define
    | Read
    | Write
    -- Symbols
    | Op Op
    | Comp Comp
    | Var String
    | Num Int
    | LPAREN
    | RPAREN
    | EOF
    deriving (Show, Eq)

-- Alex
tok :: (String -> Token) -> AlexAction Token
tok f (_,_,_,input) len = return $ f (take len input)

alexEOF :: Alex Token
alexEOF = return EOF

lexer :: String -> Either String [Token]
lexer input = runAlex input go
    where
        go = do
            output <- alexMonadScan
            if output == EOF then return [output] else (output :) <$> go
}
