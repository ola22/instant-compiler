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
type Register = String
-- (output file, last used reister's numb, {var -> register})
type Store = (FilePath, Integer, M.Map Ident Register)
type Env = StateT Store IO

{-
TODO:
potestowac na studentsie
readme
zobacztc co z tym wygenerowanym kodem tych modulow
  wywalic ta funkcje startCompilation calkiem 

zobaczyc co z tym wygenerowanym kodem tych modulow
  ZMIENIC KATALOG INSTANT NA PARSING, CZY COS???
README
  ODPALANIE
  odsmiecic katalogi projektu (np tester i z instant to tester.hs)
  MAKEA DOROBIC KTORY KOPIUJE DO KORZENIA :o
-}


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
      Bad err -> do putStrLn "\nParsing failed:"
                    putStrLn err
                    exitFailure
      Ok tree -> do (is_ok, _) <- checkCompileErrors tree S.empty
                    case is_ok of
                      False -> exitFailure
                      True -> do
                        let outfile = (dropExtension f) ++ ".ll"
                        writeFile outfile 
                          "declare void @printInt(i32)\n\ndefine i32 @main() {\n"
                        startCompilation tree outfile
                        appendFile outfile "    ret i32 0\n}\n"
                        return ()


-- Function starts compilatipn in StateT monad
startCompilation :: Program -> String -> IO ()
startCompilation p outfile = do
  evalStateT (compileTree p) (outfile, 1, M.empty)
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
  exp_val <- compileExp e
  (f, n, s) <- get
  let var_reg = M.lookup var s
  case var_reg of
    -- variable already declared
    Just r -> do 
      liftIO $ (appendFile f) $ "    store i32 " 
        ++ exp_val ++ ", i32* " ++ r ++ "\n"
      return ()
    -- variable declared for the first time
    Nothing -> do
      liftIO $ (appendFile f) $ "    %r" ++ (show n) ++ 
        " = alloca i32" ++ "\n"
      liftIO $ (appendFile f) $ "    store i32 " ++ exp_val 
        ++ ", i32* " ++ "%r" ++ (show n) ++ "\n"
      put (f, n + 1, M.insert var ("%r" ++ (show n)) s)
      return ()
compileStmt (SExp e) = do
  (f, _, _) <- get
  reg <- compileExp e
  liftIO $ (appendFile f) $ "    " ++ "call void @printInt(i32 " 
    ++ reg ++ ")\n"
  return ()


-- Function compiles single expression, it returns a register
-- in which it put result
compileExp :: Exp -> Env (Register)
compileExp (ExpAdd e1 e2) = getResultRegister "add" e1 e2
compileExp (ExpSub e1 e2) = getResultRegister "sub" e1 e2
compileExp (ExpMul e1 e2) = getResultRegister "mul" e1 e2
compileExp (ExpDiv e1 e2) = getResultRegister "sdiv" e1 e2
compileExp (ExpLit x) = return (show x)
compileExp (ExpVar var) = do
  (f, n, s) <- get
  let var_reg = M.lookup var s
  case var_reg of
    Just r -> do
      liftIO $ (appendFile f) $ "    %r" ++ 
        (show n) ++ " = load i32, i32* " ++ r ++ "\n"
      put (f, n + 1, s)
      return ("%r" ++ (show n))
    Nothing -> return ("error")


-- Function processes arithmetic operations, it returns a 
-- register in which it put result
getResultRegister :: String -> Exp -> Exp -> Env (Register)
getResultRegister s e1 e2 = do
  val1 <- compileExp e1
  val2 <- compileExp e2
  (f, n, store) <- get
  liftIO $ (appendFile f) $ "    %r" ++ (show n) ++ " = " ++ s 
    ++ " i32 " ++ val1 ++ ", " ++ val2 ++ "\n"
  put (f, n + 1, store)
  return ("%r" ++ (show n))


main :: IO ()
main = do
  args <- getArgs
  case args of
    -- given no arguments
    [] -> do
      putStrLn "Given no arguments to program, usage: insc_llvm <filename>"
    (f:[]) -> do
      getFile pProgram f
      callCommand ("llvm-as -o temp.bc " ++  (dropExtension f) ++ ".ll")
      callCommand ("llvm-link -o " ++  (dropExtension f) 
        ++ ".bc temp.bc lib/runtime.bc")
      callCommand ("rm temp.bc")
    -- given too many arguments
    _ -> do
      putStrLn "Invalid number of arguments, usage: insc_jvm <filename>"

