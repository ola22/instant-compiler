{-# Options -Wall -Wname-shadowing #-}

module ErrorCheck.ErrorChecker where

import qualified Data.Set as S

import Instant.AbsInstant


type VarsSet = S.Set Ident



-- Functions below checks if there are any compilation errors
-- (undeclared variables)
checkCompileErrors :: Program -> VarsSet -> IO ((Bool, VarsSet))
checkCompileErrors (Prog []) vars = return (True, vars)
checkCompileErrors (Prog (h:t)) vars = do 
    (b1, s1) <- checkStmt h vars
    (b2, s2) <- checkCompileErrors (Prog t) s1
    return ((b1 && b2, s2))


checkStmt :: Stmt -> VarsSet -> IO ((Bool, VarsSet))
checkStmt (SAss var e) vars = do
    (b, s) <- checkStmt (SExp e) (S.insert var vars)
    return ((b, s))
checkStmt (SExp e) vars = do
    case e of
        (ExpVar var) -> do
            let res = S.member var vars
            case res of
                True -> return (res, vars)
                False -> do putStrLn $ "\nCompilation failed: "
                                 ++ "used undeclared variable " 
                                 ++ (show var)
                            return (res, vars)
        (ExpLit _) -> return ((True, vars))
        (ExpAdd e1 e2) -> do
            (b1, _) <- checkStmt (SExp e1) vars
            (b2, _) <- checkStmt (SExp e2) vars
            return ((b1 && b2, vars))
        (ExpSub e1 e2) -> do
            (b1, _) <- checkStmt (SExp e1) vars
            (b2, _) <- checkStmt (SExp e2) vars
            return ((b1 && b2, vars))
        (ExpDiv e1 e2) -> do
            (b1, _) <- checkStmt (SExp e1) vars
            (b2, _) <- checkStmt (SExp e2) vars
            return ((b1 && b2, vars))
        (ExpMul e1 e2) -> do
            (b1, _) <- checkStmt (SExp e1) vars
            (b2, _) <- checkStmt (SExp e2) vars
            return ((b1 && b2, vars))
