module Compiler.Compile where
    import Parser.Parser
    import Compiler.Ast
    
    import Control.Monad.Except
    
    data BFInstruction = BFPlus  Int 
                       | BFMinus Int
                       | BFRight Int
                       | BFLeft  Int
                       | BFLoopL Int
                       | BFLoopR Int
                       | BFPoint Int
                       | BFComma Int
        deriving Eq

    instance Show BFInstruction where
        show i = replicate n chr
            where
                (chr, n) = case i of {
                    BFPlus n  -> ('(', n);  BFMinus n -> (')', n); 
                    BFLeft n  -> ('<', n);  BFRight n -> ('>', n);
                    BFLoopL n -> ('[', n);  BFLoopR n -> (']', n);
                    BFPoint n -> ('{', n);  BFComma n -> ('}', n);
                }

    type CompilerError = Either String

    -- compile :: Program -> [BFInstruction]
    -- compile (Program _ exp) = undefined
    compile :: Program -> CompilerError Exp
    compile = inline_defns
