{-# Options -Wall -Wname-shadowing #-}

module Main where

import System.FilePath.Posix
import System.Process
import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State

import Instant.LexInstant
import Instant.ParInstant
import Instant.AbsInstant
import Instant.ErrM
import ErrorCheck.ErrorChecker


type ParseType a = [Token] -> Err a
-- (max stack usage, max locals number, instructions, {var->ref})
type Store = (Integer, Integer, String, M.Map Ident Integer)
type Env = StateT Store IO



-- Functions below return proper jvm's instructions
-- for size of given variable
giveConst :: Integer -> String
giveConst x 
        | x == -1 = "    iconst_m1 "
        | x >= 0 && x <= 5 = "    iconst_" ++ show x
        | x >= -128 && x <= 127 = "    bipush " ++ show x
        | x >= -32768 && x <= 32767 = "    sipush " ++ show x
        | otherwise = "    ldc " ++ show x

giveLoad :: Integer -> String
giveLoad x
        | x >= 0 && x <=3 = "    iload_" ++ show x
        | otherwise = "    iload " ++ show x

giveStore :: Integer -> String
giveStore x
        | x >= 0 && x <=3 = "    istore_" ++ show x
        | otherwise = "    istore " ++ show x


-- Functions below count the maximum stack usage
countMaxStmtDepth :: Program -> Integer
countMaxStmtDepth (Prog []) = 0
countMaxStmtDepth (Prog (h:t)) = max (countStmtDepth h) 
  (countMaxStmtDepth (Prog t))

countStmtDepth :: Stmt -> Integer
countStmtDepth (SAss _ e) = countExpDepth e
countStmtDepth (SExp e) = max 2 (countExpDepth e)

countExpDepth :: Exp -> Integer
countExpDepth (ExpAdd e1 e2) = getCount e1 e2
countExpDepth (ExpSub e1 e2) = getCount e1 e2
countExpDepth (ExpMul e1 e2) = getCount e1 e2
countExpDepth (ExpDiv e1 e2) = getCount e1 e2
countExpDepth (ExpLit _) = 1
countExpDepth (ExpVar _) = 1

getCount :: Exp -> Exp -> Integer
getCount e1 e2 = 
  let res1 = countExpDepth e1
      res2 = countExpDepth e2
  in if res1 == res2 then res1 + 1
    else if res1 > res2 then res1
    else res2


-- Function returns True if given operation
-- is commutative
makeSwap :: String -> Bool
makeSwap s
        | s == "iadd" = False
        | s == "isub" = True
        | s == "imul" = False
        | s == "idiv" = True
        | otherwise = True


-- Function reads file with input program and passes
-- it to parseFunction
getFile :: ParseType Program -> FilePath -> IO ()
getFile p f = readFile f >>= parseFile p f


-- Function parses given program, then checks if there are
-- any compilation errors and finally starts compilation
parseFile :: ParseType Program -> FilePath -> String -> IO()
parseFile p f prog_s = 
  let ts = myLexer prog_s in 
    case p ts of
      -- parsing error
      Bad err -> do putStrLn "\nParsing failed:"
                    putStrLn err
                    exitFailure
      -- parsing succeded
      Ok tree -> do (is_ok, _) <- checkCompileErrors tree S.empty
                    case is_ok of
                      -- compilation error
                      False -> exitFailure
                      True -> do
                        let outfile = (dropExtension f) ++ ".j"
                        let max_stack = countMaxStmtDepth tree
                        startCompilation tree max_stack outfile
                        return ()
                        

-- Function starts compilatipn in StateT monad
startCompilation :: Program -> Integer -> FilePath -> IO ()
startCompilation p max_stack outfile = do
  evalStateT (getCompilationCode p outfile) (max_stack, 1, "", M.empty)
  return ()


-- Function writes whole output to file 
getCompilationCode :: Program -> FilePath -> Env ()
getCompilationCode p f = do
  -- adding jvm file's beginning
  liftIO $ writeFile f $ ".class public " ++ (takeBaseName f)
    ++ "\n.super java/lang/Object\n\n.method public <init>()V\n"
    ++ "    aload_0\n    invokespecial java/lang/Object/<init>()V\n"
    ++ "    return\n.end method\n"
    ++ "\n.method public static main([Ljava/lang/String;)V\n" 
  compileTree p
  (max_stack, maxlocals, res, _) <- get
  -- adding all instructions and jvm file's beginning
  liftIO $ appendFile f $ ".limit stack " ++ (show max_stack)
    ++ "\n.limit locals " ++ (show maxlocals) ++ "\n" ++ res
    ++ "    return\n.end method"
  return ()


-- Function compiles whole program, Stmt by Stmt
compileTree :: Program -> Env ()
compileTree (Prog []) = return ()
compileTree (Prog (h:t)) = do
  compileStmt h
  compileTree (Prog t)


-- Function compiles given statement
compileStmt :: Stmt -> Env ()
compileStmt (SAss var e) = do
  (_, ress') <- compileExp e
  (max_stack, l, ress, s) <- get
  let var_ref = M.lookup var s
  case var_ref of
    -- variable already declared
    Just r -> do 
      let new_res = ress ++ ress' ++ (giveStore r) ++ "\n"
      put (max_stack, l, new_res, s)
      return ()
    -- variable declared for the first time
    Nothing -> do
      let new_res = ress ++ ress' ++ (giveStore l) ++ "\n"
      put (max_stack, l + 1, new_res, M.insert var l s)
      return ()
compileStmt (SExp e) = do
  (stack, ress') <- compileExp e
  (max_stack, l, ress, s) <- get
  -- optimalizing swap's number and stack usage
  let new_ress = if stack >= max_stack then (ress ++ ress' ++ 
        "    getstatic java/lang/System/out Ljava/io/PrintStream;\n" ++
        "    swap\n" ++ "    invokevirtual java/io/PrintStream/println(I)V\n")
                else (ress ++ "    getstatic java/lang/System/out Ljava/io/PrintStream;\n"
        ++ ress' ++ "    invokevirtual java/io/PrintStream/println(I)V\n")
  put (max_stack, l, new_ress, s)
  return ()
  

-- Function compiles single expression, it returns a pair
-- (stack usage, instructions)
compileExp :: Exp -> Env (Integer, String)
compileExp (ExpAdd e1 e2) = getMaxStackAndRes "iadd" e1 e2
compileExp (ExpSub e1 e2) = getMaxStackAndRes "isub" e1 e2
compileExp (ExpMul e1 e2) = getMaxStackAndRes "imul" e1 e2
compileExp (ExpDiv e1 e2) = getMaxStackAndRes "idiv" e1 e2
compileExp (ExpLit x) = return (1, (giveConst x) ++ "\n")
compileExp (ExpVar var) = do
  (_, _, _, s) <- get
  let var_ref = M.lookup var s
  case var_ref of
    Just r -> do
      return (1, (giveLoad r) ++ "\n")
    Nothing -> return (0, "error")


-- Function processes arithmetic operations, it returns a pair
-- (stack usage, instructions)
getMaxStackAndRes :: String -> Exp -> Exp -> Env (Integer, String)
getMaxStackAndRes s e1 e2 = do
  (stack1, res1) <- compileExp e1
  (stack2, res2) <- compileExp e2
  let op = "    " ++ s ++ "\n"
  let d1 = max stack1 (stack2 + 1)
  let d2 = max stack2 (stack1 + 1)
  -- optimalizing stack usage
  return (if d1 <= d2 then (d1, res1 ++ res2 ++ op)
    else (stack2, res2 ++ res1 ++ 
      (if ((makeSwap s) == False) then "" else "    swap\n") ++ op))


main :: IO ()
main = do
  args <- getArgs
  case args of
    -- given no arguments
    [] -> do
      putStrLn "Given no arguments to program, usage: insc_jvm <filename>"
    (f:[]) -> do
      getFile pProgram f
      callCommand ("java -jar lib/jasmin.jar " ++ (dropExtension f) 
        ++ ".j -d " ++ (takeDirectory f))
    -- given too many arguments
    _ -> do
      putStrLn "Invalid number of arguments, usage: insc_jvm <filename>"
