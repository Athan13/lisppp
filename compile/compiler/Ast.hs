module Compiler.Ast where
    import Parser.Parser

    import qualified Data.Map as M
    import Control.Monad.Except

    type AstError = Either String
    
    {- Inlining -}
    replace_vars :: M.Map String Exp -> Exp -> AstError Exp
    replace_vars symtab e = case e of
        Num n -> return $ Num n
        Var s -> case M.lookup s symtab of 
            Just e  -> return e
            Nothing -> throwError $ "Unbound variable " ++ s ++ " in " ++ show e
        Op op e1 e2 -> do
            e1 <- replace_vars symtab e1
            e2 <- replace_vars symtab e2
            return $ Op op e1 e2
        Comp c e1 e2 -> do
            e1 <- replace_vars symtab e1
            e2 <- replace_vars symtab e2
            return $ Comp c e1 e2
        While e1 e2 -> do
            e1 <- replace_vars symtab e1
            e2 <- replace_vars symtab e2
            return $ While e1 e2
        Let s e1 e2 -> do
            e1 <- replace_vars symtab e1
            e2 <- replace_vars symtab e2
            return $ Let s e1 e2
        Read      -> return $ Read
        Write e   -> replace_vars symtab e >>= return . Write
        Call s es -> mapM (replace_vars symtab) es >>= (return . Call s)

    inline_defn :: Defn -> Exp -> AstError Exp
    inline_defn d@(Defn name args body) e = case e of
        Num n -> return $ Num n
        Var s -> return $ Var s
        Op op e1 e2 -> do
            e1 <- inline_defn d e1
            e2 <- inline_defn d e2
            return $ Op op e1 e2
        Comp c e1 e2 -> do
            e1 <- inline_defn d e1
            e2 <- inline_defn d e2
            return $ Comp c e1 e2
        While e1 e2 -> do
            e1 <- inline_defn d e1
            e2 <- inline_defn d e2
            return $ While e1 e2
        Let s e1 e2 -> do
            e1 <- inline_defn d e1
            e2 <- inline_defn d e2
            return $ Let s e1 e2
        Read    -> return $ Read
        Write e -> inline_defn d e >>= return . Write
        Call s es -> do 
            es <- mapM (inline_defn d) es
            if name /= s then 
                return $ Call s es
            else if length es /= length args then
                throwError $ "Argument mismatch: called function " ++ show d ++ " with arguments " ++ show es
            else replace_vars (M.fromList $ zip args es) body

    inline_defns :: Program -> AstError Exp
    inline_defns (Program [] body) = return body
    inline_defns (Program (d:ds) body) = 
        do
            ds <- mapM (update_defn d) ds
            body <- inline_defn d body
            inline_defns $ Program ds body
        where
            update_defn d (Defn name args body) = inline_defn d body >>= (return . Defn name args)
    