{-# Options -Wall -Wname-shadowing #-}
module Main where

import System.FilePath.Posix
import System.Process
import System.Environment ( getArgs )
import System.Exit ( exitFailure )

import qualified Data.Map.Strict as M
import Control.Monad.State

import Instant.LexInstant
import Instant.ParInstant
import Instant.AbsInstant
import Instant.ErrM



type ParseType a = [Token] -> Err a                  -- O CO TU WGL CHODZI????????

type Register = String
-- variable -> register
type Store = (Integer, M.Map Ident Register)

type Env = StateT Store IO

{-
TODO:
moze ten typ mieszany
nazwy wszsystkie
f do stora
ten błąd kompilacji
nazwy rejestrow
potestowac
poodpalac
na studentsie zbudowac
readme
odsmiecic katalogi projektu
zobaczyc co z tym wygenerowanym kodem tych modulow
wrzucic na gita
komantarze
czy sie dobrze odpala wszytsko
no bo tam jest tez num, a nie tylko register, moze zrobic, jak tamta dziewczyna?????
ze typ do wyboru i pattern matching???
-}



getFile :: ParseType Program -> FilePath -> IO ()
getFile p f = readFile f >>= parseFile p f


parseFile :: ParseType Program -> FilePath -> String -> IO()
parseFile p f prog_s = 
  let ts = myLexer prog_s in 
    case p ts of
      Bad err -> do putStrLn "\nParsing failed:"
                    putStrLn err
                    exitFailure
      Ok tree -> do let outfile = (dropExtension f) ++ ".ll"
                    writeFile outfile "declare void @printInt(i32)\n\ndefine i32 @main(){\n"
                    startCompilation tree outfile
                    appendFile outfile "    ret i32 0\n}\n"
                    return ()


startCompilation :: Program -> String -> IO ()
startCompilation p outfile = do
  evalStateT (compileTree p outfile) (1, M.empty)
  return ()


compileTree :: Program -> FilePath -> Env ()
compileTree (Prog []) _ = return ()
compileTree (Prog (h:t)) f = do
  compileStmt h f
  compileTree (Prog t) f


compileStmt :: Stmt -> FilePath -> Env ()
compileStmt (SAss var e) f = do
  exp_val <- compileExp e f
  (n, s) <- get
  let var_reg = M.lookup var s
  case var_reg of
    -- store i32 123, i32* %a_p
    -- %a_p = alloca i32
    Just r -> do 
      liftIO $ (appendFile f) $ "    store i32 " ++ exp_val ++ ", i32* " ++ r ++ "\n"
      return ()
    Nothing -> do
      liftIO $ (appendFile f) $ "    %r" ++ (show n) ++ " = alloca i32" ++ "\n"
      liftIO $ (appendFile f) $ "    store i32 " ++ exp_val ++ ", i32* " ++ "%r" ++ (show n) ++ "\n"
      put (n + 1, M.insert var ("%r" ++ (show n)) s)
      return ()
compileStmt (SExp e) f = do
  reg <- compileExp e f
  liftIO $ (appendFile f) $ "    " ++ "call void @printInt(i32 " ++ reg ++ ")\n"
  return ()



compileExp :: Exp -> FilePath -> Env (Register)
compileExp (ExpAdd e1 e2) f = getResultRegister "add" e1 e2 f
compileExp (ExpSub e1 e2) f = getResultRegister "sub" e1 e2 f
compileExp (ExpMul e1 e2) f = getResultRegister "mul" e1 e2 f
compileExp (ExpDiv e1 e2) f = getResultRegister "udiv" e1 e2 f
compileExp (ExpLit x) _ = return (show x)
compileExp (ExpVar var) f = do
  (n, s) <- get
  let var_reg = M.lookup var s
  case var_reg of
    -- %a.1 = load i32, i32* %a_p
    Just r -> do
      liftIO $ (appendFile f) $ "    %r" ++ (show n) ++ " = load i32, i32* " ++ r ++ "\n"
      put (n + 1, s)
      return ("%r" ++ (show n))                   -- TODO nowy rejestr, zaladowac tam to i zwrocic
    Nothing -> return ("error") --putStrLn ("\nCompilation error: used undeclared variable " + show var) TODO
                  --exitFailure

getResultRegister :: String -> Exp -> Exp -> FilePath -> Env (Register)
getResultRegister s e1 e2 f = do
  val1 <- compileExp e1 f
  val2 <- compileExp e2 f
  (n, store) <- get
  liftIO $ (appendFile f) $ "    %r" ++ (show n) ++ " = " ++ s ++ " i32 " ++ val1 ++ ", " ++ val2 ++ "\n"
  put (n + 1, store)
  return ("%r" ++ (show n))

{-pierwszy raz do zmiennej -> nowy rejestr %rej = alloca 32
a ktorys raz -> do tego samego miejsca załadowac do adresu nowa wartosc store rejestz wartosci rejestrzadresem
load -> z pamieci do rejestru
store -> z rejestru i adre i zapisuje pod sdresem te wartosc
dodawanie -> exp1 rejestr, exp2 rejestr2 plus na wynik-}


main :: IO ()
main = do
  args <- getArgs

  case args of
    -- given no arguments
    [] -> do
      putStrLn "Invalid number of arguments, usage: insc_llvm <filename>"
    file -> do
      getFile pProgram (head file)
      callCommand ("llvm-as -o temp.bc " ++  (dropExtension (head file)) ++ ".ll")
      callCommand ("llvm-link -o " ++  (dropExtension (head file)) ++ ".bc temp.bc lib/runtime.bc")
      callCommand ("rm temp.bc")