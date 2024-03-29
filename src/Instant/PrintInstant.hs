{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Instant.PrintInstant where

-- pretty-printer generated by the BNF converter

import Instant.AbsInstant
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))



instance Print Program where
  prt i e = case e of
    Prog stmts -> prPrec i 0 (concatD [prt 0 stmts])

instance Print Stmt where
  prt i e = case e of
    SAss id exp -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 exp])
    SExp exp -> prPrec i 0 (concatD [prt 0 exp])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print Exp where
  prt i e = case e of
    ExpAdd exp1 exp2 -> prPrec i 1 (concatD [prt 2 exp1, doc (showString "+"), prt 1 exp2])
    ExpSub exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "-"), prt 3 exp2])
    ExpMul exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString "*"), prt 4 exp2])
    ExpDiv exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString "/"), prt 4 exp2])
    ExpLit n -> prPrec i 4 (concatD [prt 0 n])
    ExpVar id -> prPrec i 4 (concatD [prt 0 id])


