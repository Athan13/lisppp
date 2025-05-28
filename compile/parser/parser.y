{
module Parser.Parser where

import qualified Parser.Lexer as L
import Parser.Lexer (Op)
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
    define      { L.Define }
    read        { L.Read }
    write       { L.Write }
    op          { L.Op $$ }
    eq          { L.Eq }
    neq         { L.Neq }
    lt          { L.Lt }
    lte         { L.Lte }
    gt          { L.Gt }
    gte         { L.Gte }
    not         { L.Not }
    var         { L.Var $$ }
    num         { L.Num $$ }
    lparen      { L.LPAREN }
    rparen      { L.RPAREN }
    EOF         { L.EOF }

%%
Program : Defns Exp { Program $1 $2 }

Defns : {- empty -}                                     { [] }
      | Defns lparen define var lparen Args Exp rparen  { (Defn $4 $6 $7) : $1 }

Args : rparen   { [] }
    | var Args  { $1 : $2 }

Exp : num          { Num $1 }
    | var          { Var $1 }
    | lparen op Exp Exp rparen      { Op $2 $3 $4 }
    | lparen eq Exp Exp rparen      { Eq $3 $4 }
    | lparen neq Exp Exp rparen     { Neq $3 $4 }
    | lparen lt Exp Exp rparen      { Gt $4 $3 }
    | lparen lte Exp Exp rparen     { Not (Gt $3 $4) }
    | lparen gt Exp Exp rparen      { Gt $3 $4 }
    | lparen gte Exp Exp rparen     { Not (Gt $4 $3) }
    | lparen not Exp rparen         { Not $3 }
    | lparen if Exp Exp Exp rparen  { If $3 $4 $5 }
    | lparen let lparen var Exp rparen Exp rparen { Let $4 $5 $7 }
    | lparen read rparen       { Read }
    | lparen write Exp rparen  { Write $3 }
    | lparen var CallArgs      { Call $2 $3 }

CallArgs : rparen       { [] }
         | Exp CallArgs { $1 : $2 }

{
data Program = Program [Defn] Exp
    deriving (Show)

data Defn = Defn String [String] Exp
    deriving (Show)

data Exp = Num Int
         | Var String
         | Op Op Exp Exp
         -- Boolean operations
         | Eq Exp Exp
         | Neq Exp Exp
         | Gt Exp Exp
         | Not Exp
         | If Exp Exp Exp
         | Let String Exp Exp
         | Read
         | Write Exp
         -- function name, args
         | Call String [Exp]
         -- condition, args, initial_args, new_args, return_value
         | TailCall Exp [String] [Exp] [Exp] Exp
    deriving (Show)

parseError :: L.Token -> L.Alex a
parseError token = do
  ((L.AlexPn _ line column), _, _, _) <- L.alexGetInput
  L.alexError ("parse error at line " ++ (show line) ++ ", column " ++ (show column) ++ " on token " ++ show token)

lexer :: (L.Token -> L.Alex a) -> L.Alex a
lexer = (L.alexMonadScan >>=)
}