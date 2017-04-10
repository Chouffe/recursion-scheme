{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Cata where

import           Control.Arrow    ((>>>), (<<<))
import           Data.Monoid      ((<>))
import           Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P

data Expr a
  = Literal { intVal :: Int }
  | Ident   { name :: String  }
  | Index   { target :: a, idx :: a }
  | Unary   { op :: String, target :: a }
  | Binary  { lhs :: a, op :: String, rhs :: a }
  | Call    { func :: a, args :: [a] }
  | Paren   { target :: a }
  deriving (Show, Eq, Functor)

-- Least fixed point combinator
newtype Term f = In { out :: f (Term f) }

ten :: Term Expr
ten = In (Literal { intVal = 10 })

add :: Term Expr
add = In (Ident { name = "add" })

call :: Term Expr
call = In (Call { func = add, args = [ten, ten] })

bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp f =
  out
  >>> fmap (bottomUp f)
  >>> In
  >>> f

topDown :: Functor a => (Term a -> Term a) -> Term a -> Term a
topDown f =
  f
  >>> out
  >>> fmap (topDown f)
  >>> In

mystery :: Functor f => (f b -> b) -> Term f -> b
mystery f =
  out
  >>> fmap (mystery f)
  >>> f

-- Mystery function is actually a catamorphism
cata :: Functor f => (f b -> b) -> Term f -> b
cata f = out >>> fmap (cata f) >>> f

bottomUp' :: Functor f => (Term f -> Term f) -> Term f -> Term f
bottomUp' f = cata (In >>> f)

ana :: Functor f => (a -> f a) -> a -> Term f
ana f = In <<< fmap (ana f) <<< f

countNodes :: Algebra Expr Int
countNodes (Literal _)           = 1
countNodes (Ident _)             = 1
countNodes (Index it indx)       = it + indx + 1
countNodes (Unary _ arg)         = arg + 1
countNodes (Binary left _ right) = left + right + 1
countNodes (Call fn xargs)       = fn + sum xargs + 1
countNodes (Paren arg)           = arg + 1

-- Functions of type (Functor f => f a -> a) are so ubiquitous that they are called Algebras

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

prettyPrint :: Algebra Expr Doc
prettyPrint (Literal i)              = P.int i
prettyPrint (Ident s)                = P.text s
prettyPrint (Call f as)              = f <> P.parens (P.fcat (P.punctuate "," as))
prettyPrint (Index it indx)          = it <> P.brackets indx
prettyPrint (Unary oper it)          = P.text oper <> it
prettyPrint (Binary left oper right) = left <> P.text oper <> right
prettyPrint (Paren expr)             = P.parens expr

-- > cata prettyPrint call
