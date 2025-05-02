module Examples where

import Syntax
import Embedding
import Stdlib
import Semantics

throw :: Expr -> Expr
throw = Do #error "throw"

cthrow :: Expr -> Expr
cthrow = Do #catch "throw"

catch :: Expr -> Expr -> Expr
catch comp hdl = Do #catch "catch" (Pair comp hdl)

catchExample :: Expr
catchExample =
    withStdLib $
    #case :@
    withHandler
        (#x --> #inr :@ #x)
        [("throw", #e, #k) --> #inl :@ #e]
        #error (
    withHandler
        (#x --> #x)
        [ ("catch", #args, #k) --> Embed (
            withHandler
                (#x --> #x)
                [ ("throw", #e, #k) --> Snd #args :@ #e]
                #error $
                Fst #args :@ unit
         ) #k]
        #catch $
    catch (thunk $ throw (c 1)) (Lam #e (c 41 +. #e)))
    :@ Lam #z (c 0) :@ Lam #z #z

-- Не рабоатет без рекурсивных определений
transactionalCatch :: Expr
transactionalCatch =
    withStdLib $
    (#hCatch =. Lam #comp (withHandler
        (#x --> #inr :@ #x)
        [ ("throw", #e, #k) --> #inl :@ #e
        , ("catch", #args, #k) --> Embed (
            #case :@ (#hCatch :@ thunk (Fst #args))
            :@ Lam #err (thunk $ Do #catch "abort" (#k :@ (Snd #args :@ #err)))
            :@ Lam #x #x
        ) #k
        , ("abort", #m, #_) --> force #m]
        #catch
        (force #comp)
    )) $
    withState #u (c 42) $
    #hCatch :@ thunk (
        catch (thunk $ put #u (c 1) $$ cthrow (c 2)) (thunk $ get #u))

-- такой fix разваливается в энергичной семантике
transCatchFix :: Expr
transCatchFix =
    withStdLib $
    (#hCatch =. #fix :@ Lam #rec (Lam #comp (withHandler
        (#x --> #inr :@ #x)
        [ ("throw", #e, #k) --> #inl :@ #e
        , ("catch", #args, #k) --> Embed (
            #case :@ (#rec :@ thunk (Fst #args))
            :@ Lam #err (thunk $ Do #catch "abort" (#k :@ (Snd #args :@ #err)))
            :@ Lam #x #x
        ) #k
        , ("abort", #m, #_) --> force #m]
        #catch
        (force #comp)
    ))) $
    withState #u (c 42) $
    #hCatch :@ thunk (
        catch (thunk $ put #u (c 1) $$ throw (c 2)) (thunk $ get #u))

-- почему-то не завершается
transCatchNoFix :: Expr
transCatchNoFix =
    withStdLib $
    (#hCatch =. Lam #comp (withHandler
        (#x --> #inr :@ #x)
        [ ("throw", #e, #k) --> #inl :@ #e
        , ("catch", #args, #k1) --> Embed (
            #case :@
                withHandler
                (#x --> #inr :@ #x)
                [("throw", #e, #k) --> #inl :@ #e]
                #catch
                (force $ Fst #args)
            :@ Lam #err (thunk $ Do #catch "abort" (#k1 :@ (Snd #args :@ #err)))
            :@ Lam #x #x
        ) #k
        , ("abort", #m, #_) --> force #m]
        #catch
        (force #comp)
    )) $
    withState #u (c 42) $
    #hCatch :@ thunk (
        catch (thunk $ put #u (c 1) $$ cthrow (c 2)) (thunk $ get #u))