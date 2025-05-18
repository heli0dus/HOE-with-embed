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

embedTest :: Expr
embedTest =
    withHandler
        (#x --> #x)
        [("op", #_, #k) --> c 1]
        #op $
    withHandler
        (#x --> #x)
        [("b", #_, #k) --> Embed (
          Do #op "op" unit
        ) #k]
        #b $
    withHandler
        (#x --> #x)
        [("op", #_, #k) --> c 2]
        #op $
    Do #b "b" unit

embedTest2 :: Expr
embedTest2 =
    withHandler
        (#x --> #x)
        [("op", #_, #k) --> c 1]
        #op $
    withHandler
        (#x --> #x)
        [("b", #_, #k) -->
          Do #op "op" unit
        ]
        #b $
    withHandler
        (#x --> #x)
        [("op", #_, #k) --> c 2]
        #op $
    Do #b "b" unit

embedTest3 :: Expr
embedTest3 =
    withHandler
        (#x --> #x)
        [("op", #_, #k) --> #k :@ c 1 +. #k :@ c 2 ]
        #op $
    Do #op "op" unit +. c 1

embedTest4 :: Expr
embedTest4 =
    withHandler
        (#x --> #x)
        [("op", #_, #k) --> c 1]
        #op $
    withHandler
        (#x --> #x)
        [("b", #arg, #k) --> Embed (
            withHandler
                (#x --> #x)
                [("op", #_, #k) --> c 2]
                #op $
            force #arg
        ) #k]
        #b $
    Do #b "b" (thunk $ Do #op "op" unit)

embedTest6 :: Expr
embedTest6 =
    withHandler
        (#x --> #x)
        [("op", #_, #k) --> #k :@ c 1]
        #op $
    withHandler
        (#x --> #x)
        [("b", #_, #k) --> #k :@ Do #op "op" unit
        ]
        #b $
    withHandler
        (#x --> #x)
        [("op", #_, #k) --> #k :@ c 2]
        #op $
    Do #op "op" unit +. Do #b "b" unit

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

catchExample2 :: Expr
catchExample2 =
    withStdLib $
    #case :@
    withHandler
        (#x --> #inr :@ #x)
        [ ("catch", #args, #k) --> Embed (
            withHandler
                (#x --> #x)
                [ ("throw", #e, #k) --> Snd #args :@ #e]
                #catch $
                Fst #args :@ unit
         ) #k
         , ("throw", #e, #k) --> #inl :@ #e]
        #catch (catch (thunk $ cthrow (c 1)) (Lam #e (c 41 +. #e)))
    :@ Lam #z (c 0) :@ Lam #z #z

-- Не работает без рекурсивных определений
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

transCatchFix :: Expr
transCatchFix =
    withStdLib $
    (#hCatch =. #fix :@ Lam #rec (Lam #comp (withHandler
        (#x --> #inr :@ #x)
        [ ("throw", #e, #k) --> #inl :@ #e
        , ("catch", #args, #k1) --> Embed (
            #case :@ (#rec :@ Fst #args)
            :@ Lam #err (Do #catch "abort" (thunk $ Embed (Snd #args :@ #err) #k1 ))
            :@ Lam #x #x
        ) #k1
        , ("abort", #m, #_) --> force #m]
        #catch
        (force #comp)
    ))) $
    #case :@ (
        #hCatch :@ thunk (
            withState #u (c 42) $
            catch (thunk $ put #u (c 1) $$ cthrow (c 2)) (thunk $ get #u)))
        :@ thunk (c 0) :@ Lam #z #z


-- почему-то не завершается
transCatchNoFix :: Expr
transCatchNoFix =
    withStdLib $
    #case :@
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
            :@ Lam #err (Do #catch "abort" (thunk $ Embed (Snd #args :@ #err) #k1 ))
            :@ Lam #x #x
        ) #k1
        , ("abort", #m, #_) --> force #m]
        #catch
        (force #comp)
    ))
    (#hCatch :@ thunk (
        withState #u (c 42) $
        catch (thunk $ put #u (c 1) $$ cthrow (c 2)) (thunk $ get #u)))
         :@ thunk (c 0) :@ Lam #z #z

transCatchNoFix2 :: Expr
transCatchNoFix2 =
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
            :@ Lam #err (Do #catch "abort" (thunk $ #k1 :@ (Snd #args :@ #err)))
            :@ Lam #x #x
        ) #k1
        , ("abort", #m, #_) --> force #m]
        #catch
        (force #comp)
    )) $
    #case :@ (#hCatch :@ thunk (
        catch (thunk $ cthrow (c 2)) (thunk $ c 42))) :@ (thunk $ c 0) :@ Lam #z #z

withSchedule :: Expr -> Expr
withSchedule scope =
    withState #taskQueue #nil $
    (#hSchedule =. #fix :@ Lam #rec (Lam #scope $
        withHandler
            (#x --> #x)
            [("fork", #arg, #k ) -->
                (#q =. get #taskQueue)
                (put #taskQueue (#cons :@ #k :@ #q) $$
                Embed
                (force #arg $$ Do #schedule "finish" unit)
                #k)
            , ("yield", #_, #k) -->
                (#q =. get #taskQueue)
                (#if :@ (#listIsEmpty :@ #q)
                    :@ (#k :@ unit)
                    :@ (#nq =. #listSplitLast :@ #q)
                        (put #taskQueue (#cons :@ #k :@ Snd #nq) $$
                         Fst #nq :@ unit)
                     )
            , ("finish", #_, #k) -->
                (#q =. get #taskQueue)
                (#if :@ (#listIsEmpty :@ #q)
                    :@ unit
                    :@ (#nq =. #listSplitLast :@ #q)
                        (put #taskQueue (Snd #nq) $$
                         Fst #nq :@ unit)
                     )]
            #schedule
            (force #scope)))
    (#hSchedule :@ thunk scope)

fork :: Expr -> Expr
fork = Do #schedule "fork"

concurrentExample :: Expr
concurrentExample =
    withStdLib $
    withSchedule $
    withState #x (c 0) $
        (#z =. get #x)
        (put #x (#z +. c 1) $$
        fork (thunk $
            (#z2 =. get #x)
            (put #x (#z2 +. c 1))) $$
        put #x (get #x +. c 1) $$
        get #x )

concurrentExample2 :: Expr
concurrentExample2 =
    withStdLib $
    withState #x (c 0) $
    withSchedule $
        (#z =. get #x)
        (put #x (#z +. c 1) $$
        fork (thunk $
            (#z2 =. get #x)
            (put #x (#z2 +. c 1))) $$
        put #x (get #x +. c 1) $$
        get #x )