{
module Parser.Lexer where
}

%wrapper "monad"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  let					             { tok (\_ -> Let) }
  if					             { tok (\_ -> If) }
  do					             { tok (\_ -> Do) }
  define			  		         { tok (\_ -> Define) }
  read\-byte				         { tok (\_ -> Read) }
  print       					     { tok (\_ -> Write) }
  not       					     { tok (\_ -> Not) }
  $digit+				             { tok (\s -> Num (read s)) }
  [\+\-\*\/\%]                       { tok (\s -> Op (charToOp $ head s)) }
  \=                                 { tok (\_ -> Eq)}
  (\!\=)                             { tok (\_ -> Neq)}
  \<                                 { tok (\_ -> Lt)}
  (\<\=)                             { tok (\_ -> Lte)}
  \>                                 { tok (\_ -> Gt)}
  (\>\=)                             { tok (\_ -> Gte)}
  $alpha [$alpha $digit \_ \']*		 { tok (\s -> Var s) }
  \(                                 { tok (\_ -> LPAREN) }
  \)                                 { tok (\_ -> RPAREN) }

{
-- Tokens
data Op = Add | Sub | Mul | Div | Mod
    deriving (Show, Eq)
charToOp c = case c of {'+' -> Add; '-' -> Sub; '*' -> Mul; '/' -> Div; '%' -> Mod; _ -> undefined}

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
    | Eq
    | Neq
    | Lt
    | Lte
    | Gt
    | Gte
    | Not
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
