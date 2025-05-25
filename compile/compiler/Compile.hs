module Compiler.Compile where
    import Parser.Lexer (Op(..), Comp(..))
    import Parser.Parser
    import Compiler.Ast
    
    import Control.Monad.Except
    import Control.Monad.State
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

    type Symtab = M.Map String Int

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
    mov_head n = if n < 0 then BFLeft (-n) else BFRight n
    increment_head n = if n < 0 then BFMinus n else BFPlus n
    copy_cell src dest tmp start_pos = 
        [mov_head (tmp - start_pos)]  -- go to tmp
        ++ clear_cell
        ++ [mov_head (dest - tmp)]    -- go to dest
        ++ clear_cell
        ++ [mov_head (src - dest),    -- go to src
            BFLoopL 1,
            mov_head (dest - src),    -- go to dest
            BFPlus 1,
            mov_head (tmp - dest),    -- go to tmp
            BFPlus 1,
            mov_head (src - tmp),     -- go to src
            BFMinus 1,
            BFLoopR 1,
            mov_head (tmp - src),     -- go to tmp
            BFLoopL 1,
            mov_head (src - tmp),     -- go to src
            BFPlus 1,
            mov_head (tmp - src),    -- go to tmp
            BFMinus 1,
            BFLoopR 1,
            mov_head (start_pos - tmp)
        ]
    -- x += y; y = 0;
    add_cells x_pos y_pos start_pos = [
            mov_head (y_pos - start_pos),
            BFLoopL 1,
            BFMinus 1,
            mov_head (x_pos - y_pos),
            BFPlus 1,
            mov_head (y_pos - x_pos),
            BFLoopR 1,
            mov_head (start_pos - y_pos)
        ]  
    -- x -= y; y = 0;
    sub_cells x_pos y_pos start_pos = [
            mov_head (y_pos - start_pos),
            BFLoopL 1,
            BFMinus 1,
            mov_head (x_pos - y_pos),
            BFMinus 1,
            mov_head (y_pos - x_pos),
            BFLoopR 1,
            mov_head (start_pos - y_pos)
        ]  

    type CompilerError = Either String
    type CompilerState = (Symtab, Int)
    type Compiler = ExceptT String (State CompilerState)

    compile_exp :: Symtab -> Int -> Exp -> CompilerError [BFInstruction]
    compile_exp symtab pos e = case e of
        Num n   -> return $ clear_cell ++ [BFPlus n]
        Var s   -> case M.lookup s symtab of
            Just idx -> return $ copy_cell idx pos (pos + 1) pos
            Nothing  -> throwError $ "unbound variable " ++ s ++ " in expression " ++ show e
        Op Add e1 e2 -> do
            e1 <- compile_exp symtab pos e1
            e2 <- compile_exp symtab (pos + 1) e2
            return $ e1 ++ [BFRight 1] ++ e2 ++ [BFLeft 1] ++ add_cells pos (pos + 1) pos
        Op Sub e1 e2 -> do
            e1 <- compile_exp symtab pos e1
            e2 <- compile_exp symtab (pos + 1) e2
            return $ e1 ++ [BFRight 1] ++ e2 ++ [BFLeft 1] ++ sub_cells pos (pos + 1) pos
        Op Mul e1 e2 -> do  -- calculate x * y --> [pos - 1], [x], [x], [x], [tmp], [y]
            e1 <- compile_exp symtab pos e1
            e2 <- compile_exp symtab (pos + 4) e2
            return $ 
                e1 
                ++ copy_cell pos (pos + 1) (pos + 3) pos
                ++ copy_cell pos (pos + 2) (pos + 3) pos
                ++ [BFRight 4] ++ e2  -- head is at pos + 4
                ++ [BFLoopL 1, BFMinus 1] -- outside loop: addition `y` times
                ++ add_cells pos (pos + 2) (pos + 4) -- add x to x
                ++ copy_cell (pos + 1) (pos + 2) (pos + 3) (pos + 4) -- copy x back to tmp
                ++ [BFLoopR 1, BFLeft 4]
        Read    -> return $ [BFComma 1]
        Write e -> compile_exp symtab pos e >>= return . (++ [BFPoint 1])
        _ -> throwError "Unsupported"

    compile :: Program -> CompilerError [BFInstruction]
    compile p = do
        p <- inline_defns p
        compile_exp M.empty 0 p
    
    showInstructions :: [BFInstruction] -> String
    showInstructions = concatMap show
