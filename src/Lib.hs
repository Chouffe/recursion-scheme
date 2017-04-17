-- Resource: http://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/
{-# LANGUAGE DeriveFunctor #-}

module Lib where

import           Control.Arrow

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Lit
  = StrLit String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)

data Expr
  = Index Expr Expr
  | Call Expr [Expr]
  | Unary String Expr
  | Binary Expr String Expr
  | Paren Expr
  | Literal Lit
  deriving (Show, Eq)

data Stmt
  = Break
  | Continue
  | Empty
  | IfElse Expr [Stmt] [Stmt]
  | Return (Maybe Expr)
  | While Expr [Stmt]
  | Expression Expr
  deriving (Show, Eq)

-- this would turn the expression
--    (((anArray[(10)])))
-- into
--    anArray[10]

flatten :: Expr -> Expr
-- base case: do nothing to literals
flatten (Literal i)     = Literal i

-- this is the important case: we shed the Paren constructor and just
-- apply `flatten` to its contents
flatten (Paren e)       = flatten e

-- all the other cases preserve their constructors and just apply
-- the flatten function to their children that are of type `Expr`.
flatten (Index e i)     = Index (flatten e) (flatten i)
flatten (Call e args)   = Call (flatten e) (map flatten args)
flatten (Unary op arg)  = Unary op (flatten arg)
flatten (Binary l op r) = Binary (flatten l) op (flatten r)

-- This code is oppressive, ugly and unmaintainable!!
-- 4/6 functions are dedicated to the task of ensuring that flatten properly recurses into its argument subexpressions
-- Extremely easy to make mistakes with this


applyExpr :: (Expr -> Expr) -> Expr -> Expr
-- base case: applyExpr is the identity function on constants
applyExpr _ (Literal i)     = Literal i

-- recursive cases: apply f to each subexpression
applyExpr f (Paren p)       = Paren (f p)
applyExpr f (Index e i)     = Index (f e) (f i)
applyExpr f (Call e args)   = Call (f e) (map f args)
applyExpr f (Unary op arg)  = Unary op (f arg)
applyExpr f (Binary l op r) = Binary (f l) op (f r)

-- Easier to write and maintain
-- `applyExpr` is responsible for both the base case and the simple recursive case of flattening an expression
-- But we havent improved applyExpr
flatten' :: Expr -> Expr
flatten' (Paren e) = flatten' e
flatten' x         = applyExpr flatten' x

-- Parameterized types
data Expr' a
  = Index' a a
  | Call' a [a]
  | Unary' String a
  | Binary' a String a
  | Paren' a
  | Literal' Lit
  deriving (Show, Eq, Functor)  -- fmap for free

data Term f = In (f (Term f))

out :: Term f -> f (Term f)
out (In t) = t

bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp f =
  out                   -- unpack
  >>> fmap (bottomUp f) -- recurse
  >>> In                -- repack
  >>> f                 -- apply

topDown :: Functor a => (Term a -> Term a) -> Term a -> Term a
topDown f =
  f                     -- apply
  >>> out               -- unpack
  >>> fmap (bottomUp f) -- recurse
  >>> In                -- repack

flattenTerm :: Term Expr' -> Term Expr'
flattenTerm (In (Paren' e)) = e  -- remove all Parens
flattenTerm other           = other

mflatten :: Term Expr' -> Term Expr'
mflatten = bottomUp flattenTerm
