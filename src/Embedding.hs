{-# OPTIONS_GHC "-Wno-orphans" #-}

module Embedding where

import Syntax
import Data.Proxy
import GHC.OverloadedLabels
import GHC.TypeLits

infixr 0 =.
infixr 0 -->
infixr 0 $$
infixl 6 +.

(=.) :: VarName -> Expr -> Expr -> Expr
(name =. expr) body = Lam name body :@ expr

($$) :: Expr -> Expr -> Expr
($$) stmt = "_" =. stmt

c :: Int -> Expr
c = Const

(+.) :: Expr -> Expr -> Expr
(+.) = Plus

fst' :: Expr -> Expr
fst' = Fst

snd':: Expr -> Expr
snd' = Snd

(**.) :: Expr -> Expr -> Expr
(**.) = Pair

v :: VarName -> Expr
v = Var

class LongArrow a b c | c -> a b where
  (-->) :: a -> b -> c

instance LongArrow (OpName, VarName, VarName) Expr OpHandler where
  (opName, paramName, kName) --> opBody =
    OpHandler{ opName, paramName, kName, opBody }

instance LongArrow VarName Expr (VarName, Expr) where
  (-->) = (,)

withHandler :: (VarName, Expr) -> [OpHandler] -> EffTag -> Expr -> Expr
withHandler (pureName, pureBody) hOps tag hScope =
  Handle{tag = tag, hPure = PureHandler{ pureName, pureBody }, hOps, hScope }

embed :: Expr -> Expr -> Expr
embed = Embed

instance KnownSymbol name => IsLabel name String where
  fromLabel = symbolVal $ Proxy @name

instance KnownSymbol name => IsLabel name Expr where
  fromLabel = v $ symbolVal $ Proxy @name
