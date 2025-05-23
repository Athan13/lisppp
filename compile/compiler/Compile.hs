module Compiler.Compile where
    import Parser.Lexer (Op(..), Comp(..))
    import Parser.Parser
    import Compiler.Ast
    
    import Control.Monad.Except
    import qualified Data.Map as M
    
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

    {- Macros -}
    clear_cell = [BFLoopL 1, BFMinus 1, BFLoopR 1]
    clear_n_cells_right n = concat (replicate n (clear_cell ++ [BFRight 1])) ++ [BFLeft n]

    type CompilerError = Either String
    type Symtab = M.Map String Int

    compile_exp :: Symtab -> Int -> Exp -> CompilerError [BFInstruction]
    compile_exp symtab pos e = case e of
        Num n   -> return $ clear_cell ++ [BFPlus n]
        Var s   -> case M.lookup s symtab of
            Just idx -> return $
                clear_n_cells_right 2
                ++ [
                    BFLeft (pos - idx), 
                    BFLoopL 1, 
                    BFRight (pos - idx),
                    BFPlus 1,
                    BFRight 1,
                    BFPlus 1,
                    BFLeft (pos - idx + 1),
                    BFMinus 1,
                    BFLoopR 1,

                    BFRight (pos - idx + 1),
                    BFLoopL 1,
                    BFLeft (pos - idx + 1),
                    BFPlus 1,
                    BFRight (pos - idx + 1),
                    BFMinus 1,
                    BFLoopR 1
                ]
            Nothing  -> throwError $ "unbound variable " ++ s ++ " in expression " ++ show e
        Op op e1 e2 -> do
            e1 <- compile_exp symtab pos e1
            e2 <- compile_exp symtab (pos + 1) e2
            return $
                e1 ++ [BFRight 1] ++ e2 ++ [
                    BFLoopL 1, BFMinus 1, BFLeft 1, BFPlus 1, BFRight 1, BFLoopR 1, BFLeft 1
                ]
        Read    -> return $ [BFComma 1]
        Write e -> compile_exp symtab pos e >>= return . (++ [BFPoint 1])
        _ -> throwError "Unsupported"

    compile :: Program -> CompilerError [BFInstruction]
    compile p = do
        p <- inline_defns p
        compile_exp M.empty 0 p
    
    showInstructions :: [BFInstruction] -> String
    showInstructions = concatMap show
