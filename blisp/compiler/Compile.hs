{-
    My eternal gratitude to the Esolang wiki https://esolangs.org/wiki/Brainfuck_algorithms
    for keeping me from having to figure these out myself.
-}

module Compiler.Compile where
    import Parser.Lexer (Op(..))
    import Parser.Parser
    import Compiler.Ast
    
    import Control.Monad.Except
    import Control.Monad.Reader
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
    type Compiler = ExceptT String (Reader CompilerState)

    shift_pos :: Int -> CompilerState -> CompilerState
    shift_pos n (s, p) = (s, p + n)

    add_to_symtab :: String -> Int -> CompilerState -> CompilerState
    add_to_symtab var val (s, p) = (M.insert var val s, p)

    add_many_to_symtab :: [String] -> [Int] -> CompilerState -> CompilerState
    add_many_to_symtab vars vals (s, p) = (M.union (M.fromList $ zip vars vals) s, p)

    compile_exp :: Exp -> Compiler [BFInstruction]
    compile_exp e = case e of
        Num n -> return $ clear_cell ++ [BFPlus n]
        Var s -> do
            (symtab, pos) <- lift $ ask
            case M.lookup s symtab of
                Just idx -> return $ copy_cell idx pos (pos + 1) pos
                Nothing  -> throwError $ "unbound variable " ++ s ++ " in expression " ++ show e
        Op Add e1 e2 -> do
            (_, pos) <- lift $ ask
            e1 <- compile_exp e1
            e2 <- local (shift_pos 1) (compile_exp e2)
            return $ e1 ++ [BFRight 1] ++ e2 ++ [BFLeft 1] ++ add_cells pos (pos + 1) pos
        Op Sub e1 e2 -> do
            (_, pos) <- lift $ ask
            e1 <- compile_exp e1
            e2 <- local (shift_pos 1) (compile_exp e2)
            return $ e1 ++ [BFRight 1] ++ e2 ++ [BFLeft 1] ++ sub_cells pos (pos + 1) pos
        Op Mul e1 e2 -> do  
            {-
                temp0[-]
                temp1[-]
                x[temp1+x-]
                temp1[
                    y[x+temp0+y-]
                    temp0[y+temp0-]
                temp1-]

            [pos-1][x][y][temp0][temp1]
            -}
            e1 <- compile_exp e1
            e2 <- local (shift_pos 1) (compile_exp e2)
            return $ e1 ++ [BFRight 1] ++ e2 ++ [BFRight 1] ++ clear_n_cells_right 2
                ++ [BFLeft 2, BFLoopL 1, BFRight 3, BFPlus 1, BFLeft 3, BFMinus 1, BFLoopR 1]  -- x
                ++ [BFRight 3, BFLoopL 1]  -- temp1
                    ++ [BFLeft 2, BFLoopL 1, BFLeft 1, BFPlus 1, BFRight 2, BFPlus 1, BFLeft 1, BFMinus 1, BFLoopR 1]
                    ++ [BFRight 1, BFLoopL 1, BFLeft 1, BFPlus 1, BFRight 1, BFMinus 1, BFLoopR 1]
                ++ [BFRight 1, BFMinus 1, BFLoopR 1]
                ++ [BFLeft 3]
        Op Div e1 e2 -> do
            {-
                # >n d
                [->-[>+>>]
                    >[+[-<+>]
                    >+>>]<<<<<
                ]
                # >0 d-n%d n%d n/d
            -}
            (_, pos) <- lift $ ask
            e1 <- compile_exp e1
            e2 <- local (shift_pos 1) (compile_exp e2)
            return $ e1 ++ [BFRight 1] ++ e2 ++ [BFLeft 1] ++ clear_n_cells_right 3
                ++ [BFLoopL 1, BFMinus 1, BFRight 1, BFMinus 1, BFLoopL 1, BFRight 1, BFPlus 1, BFRight 2, BFLoopR 1]
                ++ [BFRight 1, BFLoopL 1, BFPlus 1, BFLoopL 1, BFMinus 1, BFLeft 1, BFPlus 1, BFRight 1, BFLoopR 1]
                ++ [BFRight 1, BFPlus 1, BFRight 2, BFLoopR 1, BFLeft 5, BFLoopR 1]
                ++ copy_cell (pos + 3) pos (pos + 1) pos 
        Op Mod e1 e2 -> do
            {-
                # 0 >n d 0 0 0
                [>->+<[>]
                >[<+>-]
                <<[<]>-]
                # 0 >0 d-n%d n%d 0 0
            -}
            (_, pos) <- lift $ ask
            e1 <- compile_exp e1
            e2 <- local (shift_pos 1) (compile_exp e2)
            return $ e1 ++ [BFRight 1] ++ e2 ++ [BFRight 1] ++ clear_n_cells_right 3 ++ [BFLeft 2]
                ++ [BFLoopL 1, BFRight 1, BFMinus 1, BFRight 1, BFPlus 1, BFLeft 1, BFLoopL 1, BFRight 1, BFLoopR 1]
                ++ [BFRight 1, BFLoopL 1, BFLeft 1, BFPlus 1, BFRight 1, BFMinus 1, BFLoopR 1]
                ++ [BFLeft 2, BFLoopL 1, BFLeft 1, BFLoopR 1, BFRight 1, BFMinus 1, BFLoopR 1]
                ++ copy_cell (pos + 2) pos (pos + 1) pos
        Eq e1 e2 -> do 
            -- x[-y-x]+y[x-y[-]]x for [x = pos][y = pos + 1][pos + 2]...
            e1 <- compile_exp e1
            e2 <- local (shift_pos 1) (compile_exp e2)
            return $ e1 ++ [BFRight 1] ++ e2 ++ [BFLeft 1]
                ++ [BFLoopL 1, BFMinus 1, BFRight 1, BFMinus 1, BFLeft 1, BFLoopR 1, BFPlus 1]
                ++ [BFRight 1, BFLoopL 1, BFLeft 1, BFMinus 1, BFRight 1]
                ++ clear_cell ++ [BFLoopR 1, BFLeft 1]
        Neq e1 e2 -> do
            -- x[y-x-]y[[-]x+y]x for [x = pos][y = pos + 1][pos + 2]...
            e1 <- compile_exp e1
            e2 <- local (shift_pos 1) (compile_exp e2)
            return $ e1 ++ [BFRight 1] ++ e2 ++ [BFLeft 1]
                ++ [BFLoopL 1, BFRight 1, BFMinus 1, BFLeft 1, BFMinus 1, BFLoopR 1, BFRight 1]
                ++ [BFLoopL 2, BFMinus 1, BFLoopR 1, BFLeft 1, BFPlus 1, BFRight 1, BFLoopR 1, BFLeft 1]
        Gt e1 e2 -> do
            {-
                [pos - 1][x][y][tmp0][tmp1][z]

                temp0[-]temp1[-]z[-]
                x[ temp0+
                    y[- temp0[-] temp1+ y]
                    temp0[- z+ temp0]
                    temp1[- y+ temp1]
                y- x- ]
            -}
            (_, pos) <- lift $ ask
            e1 <- compile_exp e1
            e2 <- local (shift_pos 1) (compile_exp e2)
            return $ e1 ++ [BFRight 1] ++ e2 ++ [BFRight 1] ++ clear_n_cells_right 3 ++ [BFLeft 2] -- x
                ++ [BFLoopL 1, BFRight 2, BFPlus 1]
                    ++ [BFLeft 1, BFLoopL 1, BFMinus 1, BFRight 1] ++ clear_cell ++ [BFRight 1, BFPlus 1, BFLeft 2, BFLoopR 1]
                    ++ [BFRight 1, BFLoopL 1, BFMinus 1, BFRight 2, BFPlus 1, BFLeft 2, BFLoopR 1]
                    ++ [BFRight 1, BFLoopL 1, BFMinus 1, BFLeft 2, BFPlus 1, BFRight 2, BFLoopR 1]
                ++ [BFLeft 2, BFMinus 1, BFLeft 1, BFMinus 1, BFLoopR 1]
                ++ copy_cell (pos + 4) pos (pos + 1) pos
        Not e1 -> do
            e1 <- compile_exp e1
            return $ e1 ++ [BFRight 1] ++ clear_cell ++ [BFPlus 1]
                ++ [BFLeft 1, BFLoopL 1] ++ clear_cell ++ [BFRight 1, BFMinus 1, BFLeft 1, BFLoopR 1]
                ++ [BFRight 1, BFLoopL 1, BFMinus 1, BFLeft 1, BFPlus 1, BFRight 1, BFLoopR 1, BFLeft 1]
        If cond e1 e2 -> do
            -- [cond = pos][temp0][temp1][execute e1 or e2]
            (_, pos) <- lift $ ask
            cond <- compile_exp cond
            e1 <- local (shift_pos 1) (compile_exp e1)  -- then-case
            e2 <- local (shift_pos 2) (compile_exp e2)  -- else-case
            return $ cond ++ [BFRight 1] ++ clear_cell ++ [BFPlus 1] ++ [BFRight 1] ++ clear_cell ++ [BFLeft 2]  -- initialise temp0 = 1, temp1 = 0
                ++ [BFLoopL 1, BFRight 3] ++ e1 ++ [BFLeft 2, BFMinus 1, BFLoopR 1, BFRight 1]  -- then-case
                ++ [BFLoopL 1, BFRight 2] ++ e2 ++ [BFLeft 2, BFMinus 1, BFRight 1, BFLoopR 1, BFLeft 2]  -- else-case
                ++ copy_cell (pos + 3) pos (pos + 1) pos
        Let var e1 e2 -> do
            (_, pos) <- lift $ ask
            e1 <- compile_exp e1
            e2 <- local (add_to_symtab var pos) (compile_exp e2)
            return $ e1 ++ [BFRight 1] ++ e2 ++ copy_cell (pos + 1) pos (pos + 2) (pos + 1) ++ [BFLeft 1]
        Do es -> do
            es <- mapM compile_exp es
            return $ concat es
        Read     -> return $ [BFComma 1]
        Write e  -> compile_exp e >>= return . (++ [BFPoint 1])
        Call s _ -> throwError $ "function " ++ s ++ " is (not tail-) recursive or has not been defined."
        TailCall condition arg_names initial_args new_args return_value -> do
            (_, pos) <- lift $ ask
            let num_args = length arg_names
            let arg_posns = [pos..]
            initial_args <- mapM 
                (\(offset, e) -> local (shift_pos offset) (compile_exp e)) 
                (zip [0..] initial_args)
            condition <- local (add_many_to_symtab arg_names arg_posns . shift_pos num_args) (compile_exp condition)
            new_args <- mapM 
                (\(offset, e) -> local (add_many_to_symtab arg_names arg_posns . shift_pos (num_args + offset)) (compile_exp e)) 
                (zip [0..] new_args)
            return_value <- local (add_many_to_symtab arg_names arg_posns . shift_pos num_args) (compile_exp return_value)
            return $ concat ((++ [BFRight 1]) <$> initial_args) ++ condition
                ++ [BFLoopL 1] 
                    ++ concat ((++ [BFRight 1]) <$> new_args)
                    ++ concatMap  -- copy all the cells to the right position
                        (\i -> copy_cell (pos + num_args + i) (pos + i) (pos + 2 * num_args + 1) (pos + 2 * num_args))
                        [(num_args - 1),(num_args - 2)..0]
                    ++ [BFLeft num_args]
                    ++ condition 
                ++ [BFLoopR 1] ++ return_value
                ++ copy_cell (pos + num_args) pos (pos + 1) (pos + num_args) 
                ++ [BFLeft num_args]

    compile :: Program -> CompilerError [BFInstruction]
    compile p = do
        p <- inline_defns p
        result <- runReader (runExceptT (compile_exp p)) (M.empty, 0)
        return result

    showInstructions :: [BFInstruction] -> String
    showInstructions = concatMap show
