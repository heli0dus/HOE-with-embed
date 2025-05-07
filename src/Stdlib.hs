module Stdlib where

import Syntax
import Embedding

withStdLib :: Expr -> Expr
withStdLib =
  (#nil =. Lam #s $ Lam #z #z) .
  (#cons =. Lam #x $ Lam #xs $ Lam #s $ Lam #z $ #s :@ #x :@ (#xs :@ #s :@ #z)) .
  (#concat =. Lam #xs $ Lam #ys $ Lam #s $ Lam #z $ #xs :@ #s :@ (#ys :@ #s :@ #z)) .
  (#plus =. Lam #x $ Lam #y $ #x +. #y) .
  (#sum =. Lam #xs $ #xs :@ #plus :@ c 0) .
  (#inl =. Lam #x $ Lam #f $ Lam #g $ #f :@ #x) .
  (#inr =. Lam #x $ Lam #f $ Lam #g $ #g :@ #x) .
  (#case =. Lam #variant $ Lam #f $ Lam #g $ #variant :@ #f :@ #g) .
  (#true =. Lam #x $ Lam #y #x) .
  (#false =. Lam #x $ Lam #y #y) .
  (#if =. Lam #x $ #x) .
  (#fix =. Lam #f $ Lam #x (#f :@ Lam #z ((#x :@ #x) :@ #z)) :@ Lam #x (#f :@ Lam #z ((#x :@ #x) :@ #z))) .
  (#listSplit =. Lam #xs $ #xs :@ Lam #x (Lam #xs' $ #case :@ Fst #xs' :@ thunk (Pair (#inr :@ #x) #nil) :@ Lam #val (Pair (#inr :@ #x) (#cons :@ #val :@ Snd #xs'))) :@ Pair (#inl :@ unit) #nil) .
  (#listSplitLast =. Lam #xs $ Lam #res  (Snd #res) :@
    (#xs :@ Lam #elem (Lam #pr $ #if :@ Fst #pr :@
        Pair #true (Pair (Fst $ Snd #pr ) (#cons :@ #elem :@ Snd (Snd #pr)))
        :@ Pair #true (Pair #elem (Snd (Snd #pr))))
      :@ Pair #false (Pair unit #nil) )) .
  (#listIsEmpty =. Lam #xs $ #xs :@ thunk (thunk #false) :@ #true)


withState :: OpName -> Expr -> Expr -> Expr
withState name ini scope =
  withHandler
    (#x --> Lam #s $ #x)
    [ ("get(" <> name <> ")", #_, #k) --> Lam #s $ #k :@ #s :@ #s
    , ("put(" <> name <> ")", #s', #k) --> Lam #s $ #k :@ #s :@ #s'
    ]
    #state
    scope
  :@ ini

get :: String -> Expr
get name = Do #state ("get(" <> name <> ")") (c 0)

put :: String -> Expr -> Expr
put name = Do #state ("put(" <> name <> ")")

thunk :: Expr -> Expr
thunk = Lam "_"

force :: Expr -> Expr
force = (:@ unit)

do' :: OpName -> EffTag -> Expr
do' tag name  = Do tag name unit

unit :: Expr
unit = c 0
