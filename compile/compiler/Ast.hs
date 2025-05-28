module Compiler.Ast where
    import Parser.Parser

    import qualified Data.Map as M
    import Control.Monad.Except

    type AstError = Either String
    
    {- Replace tail-recursive functions -}
    is_simple :: Exp -> Bool
    is_simple exp = case exp of
        Num _ -> True
        Var _ -> True
        Op _ e1 e2    -> is_simple e1 && is_simple e2
        Eq e1 e2      -> is_simple e1 && is_simple e2
        Neq e1 e2     -> is_simple e1 && is_simple e2
        Gt e1 e2      -> is_simple e1 && is_simple e2
        Not e1        -> is_simple e1
        If cond e1 e2 -> is_simple cond && is_simple e1 && is_simple e2
        Let _ e1 e2   -> is_simple e1 && is_simple e2
        Do es         -> all is_simple es
        Read          -> True
        Write e1      -> is_simple e1
        Call _ _      -> False
        TailCall _ _ _ _ _ -> False

    get_tail_call :: Defn -> Maybe ([Exp] -> Exp)
    get_tail_call (Defn d_name arg_names (If cond (Call c_name new_args) else_case)) = 
        if d_name == c_name && is_simple else_case && all is_simple new_args then
            Just $ \initial_args -> TailCall cond arg_names initial_args new_args else_case
        else Nothing
    get_tail_call (Defn d_name arg_names (If cond then_case (Call c_name new_args))) =
        if d_name == c_name && is_simple then_case && all is_simple new_args then
            Just $ \initial_args -> TailCall cond arg_names initial_args new_args then_case
        else Nothing
    get_tail_call _ = Nothing

    {- Inlining -}
    replace_vars :: M.Map String Exp -> Exp -> AstError Exp
    replace_vars symtab e = case e of
        Num n -> return $ Num n
        Var s -> case M.lookup s symtab of 
            Just e  -> return e
            Nothing -> throwError $ "unbound variable " ++ s ++ " in " ++ show e
        Op op e1 e2 -> do
            e1 <- replace_vars symtab e1
            e2 <- replace_vars symtab e2
            return $ Op op e1 e2
        Eq e1 e2 -> do
            e1 <- replace_vars symtab e1
            e2 <- replace_vars symtab e2
            return $ Eq e1 e2
        Neq e1 e2 -> do
            e1 <- replace_vars symtab e1
            e2 <- replace_vars symtab e2
            return $ Neq e1 e2
        Gt e1 e2 -> do
            e1 <- replace_vars symtab e1
            e2 <- replace_vars symtab e2
            return $ Gt e1 e2
        Not e1 -> do
            e1 <- replace_vars symtab e1
            return $ Not e1
        If cond e1 e2 -> do
            cond <- replace_vars symtab cond
            e1 <- replace_vars symtab e1
            e2 <- replace_vars symtab e2
            return $ If cond e1 e2
        Let s e1 e2 -> do
            e1 <- replace_vars symtab e1
            e2 <- replace_vars symtab e2
            return $ Let s e1 e2
        Do es -> do
            es <- mapM (replace_vars symtab) es
            return $ Do es
        Read      -> return $ Read
        Write e   -> replace_vars symtab e >>= return . Write
        Call s es -> mapM (replace_vars symtab) es >>= (return . Call s)
        TailCall condition args initial_args new_args return_value -> do
            condition <- replace_vars symtab condition
            initial_args <- mapM (replace_vars symtab) initial_args
            new_args <- mapM (replace_vars symtab) new_args
            return_value <- replace_vars symtab return_value
            return $ TailCall condition args initial_args new_args return_value

    inline_defn :: Defn -> Maybe ([Exp] -> Exp) -> Exp -> AstError Exp
    inline_defn d@(Defn name args body) is_tail_call e = case e of
        Num n -> return $ Num n
        Var s -> return $ Var s
        Op op e1 e2 -> do
            e1 <- inline_defn d is_tail_call e1
            e2 <- inline_defn d is_tail_call e2
            return $ Op op e1 e2
        Eq e1 e2 -> do
            e1 <- inline_defn d is_tail_call e1
            e2 <- inline_defn d is_tail_call e2
            return $ Eq e1 e2
        Neq e1 e2 -> do
            e1 <- inline_defn d is_tail_call e1
            e2 <- inline_defn d is_tail_call e2
            return $ Neq e1 e2
        Gt e1 e2 -> do
            e1 <- inline_defn d is_tail_call e1
            e2 <- inline_defn d is_tail_call e2
            return $ Gt e1 e2
        Not e1 -> do
            e1 <- inline_defn d is_tail_call e1
            return $ Not e1
        Let s e1 e2 -> do
            e1 <- inline_defn d is_tail_call e1
            e2 <- inline_defn d is_tail_call e2
            return $ Let s e1 e2
        If cond e1 e2 -> do
            cond <- inline_defn d is_tail_call cond
            e1 <- inline_defn d is_tail_call e1
            e2 <- inline_defn d is_tail_call e2
            return $ If cond e1 e2
        Do es -> do
            es <- mapM (inline_defn d is_tail_call) es
            return $ Do es
        Read    -> return $ Read
        Write e -> inline_defn d is_tail_call e >>= return . Write
        Call s es -> do
            es <- mapM (inline_defn d is_tail_call) es
            if name /= s then 
                return $ Call s es
            else if length es /= length args then
                throwError $ "argument mismatch, called function " ++ show d ++ " with arguments " ++ show es
            else case is_tail_call of 
                Just tail_call -> return $ tail_call es
                Nothing -> replace_vars (M.fromList $ zip args es) body
        TailCall _ _ _ _ _ -> throwError $ "invalid tailcall"

    inline_defns :: Program -> AstError Exp
    inline_defns (Program [] body) = return body
    inline_defns (Program (d:ds) body) = 
        do
            ds <- mapM (update_defn d) ds
            body <- inline_defn d tc body
            inline_defns $ Program ds body
        where
            update_defn d (Defn name args body) = inline_defn d tc body >>= (return . Defn name args)
            tc = get_tail_call d
    