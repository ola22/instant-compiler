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

type ParseType a = [Token] -> Err a                  -- O CO TU WGL CHODZI????????
-- type Store = (Integer, Integer, resstring, M.Map)
-- limitStack, maxlocals, resstring, {var->ref}
type Store = (Integer, Integer, String, M.Map Ident Integer)
--type Env = StateT Store IO
type Env = StateT Store IO

{-
makefile
to z tym odpalaniem, binarka itd
chyba wywalimy max stack ze srodowiska:o 
-}


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


makeSwap :: String -> Bool
makeSwap s
        | s == "iadd" = False
        | s == "isub" = True
        | s == "imul" = False
        | s == "idiv" = True
        | otherwise = True


countMaxStmtDepth :: Program -> Integer
countMaxStmtDepth (Prog []) = 0
countMaxStmtDepth (Prog (h:t)) = max (countStmtDepth h) (countMaxStmtDepth (Prog t))

countStmtDepth :: Stmt -> Integer
countStmtDepth (SAss _ e) = countExpDepth e
countStmtDepth (SExp e) = max 2 (countExpDepth e)

countExpDepth :: Exp -> Integer
countExpDepth (ExpAdd e1 e2) = getCount e1 e2
countExpDepth (ExpSub e1 e2) = getCount e1 e2
countExpDepth (ExpMul e1 e2) = getCount e1 e2
countExpDepth (ExpDiv e1 e2) = getCount e1 e2
countExpDepth (ExpLit x) = 1
countExpDepth (ExpVar var) = 1

getCount :: Exp -> Exp -> Integer
getCount e1 e2 = 
  let res1 = countExpDepth e1
      res2 = countExpDepth e2
  in if res1 == res2 then res1 + 1
    else if res1 > res2 then res1
    else res2



getFile :: ParseType Program -> FilePath -> IO ()
getFile p f = readFile f >>= parseFile p f


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
                        let outfile = (dropExtension f) ++ ".j"
                        let max_stack = countMaxStmtDepth tree                        -- TU ZMINANA
                        startCompilation tree max_stack outfile                        -- TU ZMIANA
                        return ()
                        

startCompilation :: Program -> Integer -> FilePath -> IO ()                          -- TU ZMIANA
startCompilation p stack outfile = do
  evalStateT (getCompilationCode p stack outfile) (1, 1, "", M.empty)
  return ()


getCompilationCode :: Program -> Integer -> FilePath -> Env ()
getCompilationCode p max_stack f = do
  -- pocztek do pliku
  liftIO $ writeFile f $ ".class public " ++ (takeBaseName f)
    ++ "\n.super java/lang/Object\n\n.method public <init>()V\n"
    ++ "    aload_0\n    invokespecial java/lang/Object/<init>()V\n"
    ++ "    return\n.end method\n"
    ++ "\n.method public static main([Ljava/lang/String;)V\n"
  compileTree p max_stack
  (stack, maxlocals, res, _) <- get
  -- koniec do pliku i wszytskie instaurkcje
  liftIO $ appendFile f $ ".limit stack " ++ (show max_stack)
    ++ "\n.limit locals " ++ (show maxlocals) ++ "\n" ++ res
    ++ "    return\n.end method"
  return ()


compileTree :: Program -> Integer -> Env ()
compileTree (Prog []) _ = return ()
compileTree (Prog (h:t)) stack = do
  compileStmt h stack
  compileTree (Prog t) stack


-- returns i monad maxstack, resstring
compileStmt :: Stmt -> Integer -> Env ()
compileStmt (SAss var e) _ = do
  (max_stack', ress') <- compileExp e
  (max_stack, l, ress, s) <- get
  let var_ref = M.lookup var s
  case var_ref of
    Just r -> do 
      let new_res = ress ++ ress' ++ (giveStore r) ++ "\n"
      put (max max_stack max_stack', l, new_res, s)
      return ()
    Nothing -> do
      let new_res = ress ++ ress' ++ (giveStore l) ++ "\n"
      put (max max_stack max_stack', l + 1, new_res, M.insert var l s)
      return ()
compileStmt (SExp e) stack = do
  (max_stack', ress') <- compileExp e
  (max_stack, l, ress, s) <- get
  -- wypisac
  {-let new_ress = ress ++ "    getstatic java/lang/System/out Ljava/io/PrintStream;\n"
          ++ ress' ++ "    invokevirtual java/io/PrintStream/println(I)V\n"                 -- czemu oni robia tu swapa?????????????
  -}
  let new_ress = if max_stack' >= stack then (ress
          ++ ress' ++ "    getstatic java/lang/System/out Ljava/io/PrintStream;\n" ++
          "    swap\n" ++ "    invokevirtual java/io/PrintStream/println(I)V\n")
                else (ress ++ "    getstatic java/lang/System/out Ljava/io/PrintStream;\n"
          ++ ress' ++ "    invokevirtual java/io/PrintStream/println(I)V\n")

  {-let new_ress = ress     -- TODO IF glebokosc jest 1
          ++ ress' ++ "    getstatic java/lang/System/out Ljava/io/PrintStream;\n" ++
          "    swap\n" ++ "    invokevirtual java/io/PrintStream/println(I)V\n"-}
  let max_stack'' = max 2 max_stack'
  put (max max_stack max_stack'', l, new_ress, s)
  --put (max max_stack (max_stack' + 1), l, new_ress, s)
  return ()
  

compileExp :: Exp -> Env (Integer, String)  -- byc moze cos zwraca
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


getMaxStackAndRes :: String -> Exp -> Exp -> Env (Integer, String)
getMaxStackAndRes s e1 e2 = do
  (stack1, res1) <- compileExp e1
  (stack2, res2) <- compileExp e2
  (max_stack, l, ress, store) <- get
  let op = "    " ++ s ++ "\n"
  {-if stack1 == stack2 then (return (stack1 + 1, res1 ++ res2 ++ op))
  else ()-}
  --let ss = if ((makeSwap s) == False) then "" else "    swap\n"
  return (if stack1 == stack2 then (stack1 + 1, res1 ++ res2 ++ op)
    else if stack1 > stack2 then (stack1, res1 ++ res2 ++ op)
    else (stack2, res2 ++ res1 ++ (if ((makeSwap s) == False) then "" else "    swap\n") ++ op))
  
  {-if (max stack1 (stack2 + 1) > max stack2 (stack1 + 1)) then 
    (return (stack1, res1 ++ res2 ++ op))
  else
    (return (stack2,  res2 ++ res1 ++ (if (makeSwap s) == True then "    swap\n" else "") ++ op))  -- niepotrzebne swapy bo pewnie to nie warunek zamiany
  -}
  --return (get)

main :: IO ()
main = do
  args <- getArgs

  case args of
    -- given no arguments
    [] -> do
      putStrLn "Invalid number of arguments, usage: insc_llvm <filename>"
    file -> do
      getFile pProgram (head file)
      --callProcess "java" ["-jar", "libs/jasmin.jar", extensionToJ $ head fs, "-d", takeDirectory $ head fs]
      callCommand ("java -jar lib/jasmin.jar " ++ (dropExtension $ head file) ++ ".j -d " ++ (takeDirectory $ head file))



{-
mamy dwa poddrzewa i ktore najpierw, ktore potem
lewe stos d1
prawe stos d2
wynik
rozmiar stosu : max d1 (d2+1)

co by sie dzialo gdyby najpierw drugie:
max d2, d1+1    !!

jak max (d1, d2+1) > niz drugi  to teraz + i * sa przemienne


zmienne:
referencja i laduje z referencji o danym numerze -> daklaracja zmiennej to referencja do środowiska kolejne liczby naturalne
deklaracja referencja trzyma liczbe istore (zapsiuje liczbe z gory stosu do) -> assignment (push na stos warotosc, a istore do ref)
odwolanei do zmiennej iload

java bitecode inst listings
-}