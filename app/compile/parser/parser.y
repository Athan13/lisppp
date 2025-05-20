{
module Parser.Parser where

import qualified Parser.Lexer as L
}

%name blisp
%tokentype { L.Token }
%error { parseError }
%monad { L.Alex } { >>= } { return }
%lexer { lexer } { L.EOF }

%token
    let         { L.Let }
    if          { L.If }
    do          { L.Do }
    while       { L.While }
    define      { L.Define }
    read        { L.Read }
    write       { L.Write }
    op          { L.Op $$ }
    comp        { L.Comp $$ }
    var         { L.Var $$ }
    int         { L.Int $$ }
    lparen      { L.LPAREN }
    rparen      { L.RPAREN }
    EOF         { L.EOF }

%%
Program : Exp { Program NilDefn $1 }
-- Program : Defns Exp EOF { Program $1 $2 }

-- Defns : {- empty -}                                 { NilDefn }
--       | Defns lparen define lparen Args Exp rparen  { Defns (Defn $5 $6) $1 }

-- Args : rparen   { NilArg }
--     | var Args  { Args $1 $2 }

Exp : int          { Int $1 }
    | var          { Var $1 }
    | lparen op Exp Exp rparen     { Op $2 $3 $4 }
    | lparen comp Exp Exp rparen   { Comp $2 $3 $4 }
    | lparen while Exp Exp rparen  { While $3 $4 }
    | lparen let lparen var Exp rparen Exp rparen { Let $4 $5 $7 }
    | lparen read rparen       { Read }
    | lparen write Exp rparen  { Write $3 }

{
data Program = Program Defns Exp
    deriving (Show)

data Defns = Defns Defn Defns | NilDefn
    deriving (Show)

data Defn = Defn Args Exp
    deriving (Show)

data Args = Args String Args | NilArg
    deriving (Show)

data Exp = Int Int
         | Var String
         | Op L.Op Exp Exp
         | Comp L.Comp Exp Exp
         | While Exp Exp
         | Let String Exp Exp
         | Read
         | Write Exp
    deriving (Show)

parseError :: L.Token -> L.Alex a
parseError _ = do
  ((L.AlexPn _ line column), _, _, _) <- L.alexGetInput
  L.alexError ("parse error at line " ++ (show line) ++ ", column " ++ (show column))

lexer :: (L.Token -> L.Alex a) -> L.Alex a
lexer = (L.alexMonadScan >>=)
}