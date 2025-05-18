import Test.HUnit

import Syntax
import Embedding
import Stdlib
import Semantics
import Examples (catchExample, transactionalCatch, transCatchFix, transCatchNoFix, catchExample2, embedTest, transCatchNoFix2, embedTest2, embedTest3, embedTest4, embedTest6, concurrentExample, concurrentExample2)

main :: IO ()
main = runTestTTAndExit $ TestList
  [ TestCase $ assertEqual "substitution" (Number 6) $ eval $
      (#x =. c 1 +. c 2) $
      (#y =. #x +. c 3)
      #y
  , TestCase $ assertEqual "lexical binding" (Number 1) $ eval $
      Lam #x (Lam #y #x) :@ c 1 :@ c 2
  , TestCase $ assertEqual "abortive effect" (Number 42) $ eval $
      withHandler
        (#x --> #x)
        [(#throw, #p, #_) --> #p] 
        #error $
      (#x =. c 1) $
      (#y =. c 41) $
      Do #error #throw (#x +. #y)
  , TestCase $ assertEqual "many perform" (Number 20) $ eval $
      withHandler
        (#x --> #x)
        [(#ask, #_, #k) --> #k :@ c 10] 
        #reader $
      do' #reader #ask +. do' #reader #ask
  , TestCase $ assertEqual "many effects in stack" (Number 33) $ eval $
      withHandler
        (#x --> #x)
        [(#ask1, #_, #k) --> #k :@ c 3] 
        #reader1 $
      withHandler
        (#x --> #x)
        [(#ask2, #_, #k) --> #k :@ c 30] 
        #reader2 $
      do' #reader1 #ask1 +. do' #reader2 #ask2
  , TestCase $ assertEqual "handler performs" (Number 13) $ eval $
      withHandler
        (#x --> #x)
        [(#ask1, #_, #k) --> #k :@ c 3] 
        #reader1 $
      withHandler
        (#x --> #x)
        [(#ask2, #_, #k) --> #k :@ (do' #reader1 #ask1 +. c 10)] 
        #reader2 $
      do' #reader2 #ask2
  , TestCase $ assertEqual "pure works" (Number 21) $ eval $
      withHandler
        (#x --> #x +. c 1)
        [(#ask, #_, #k) --> #k :@ c 10] 
        #reader $
      do' #reader #ask +. do' #reader #ask
  , TestCase $ assertEqual "pure with many handlers" (Number 1111) $ eval $
      withHandler
        (#x --> #x +. c 1)
        [(#ask1, #_, #k) --> #k :@ c 10] 
        #reader1 $
      withHandler
        (#x --> #x +. c 1000)
        [(#ask2, #_, #k) --> #k :@ c 100] 
        #reader2 $
      do' #reader1 #ask1 +. do' #reader2 #ask2
  , TestCase $ assertEqual "get + put" (Number 42) $ eval $
      withState #x (c 10) $
      (#tmp =. get #x) $
      put #x (#tmp +. c 32) $$
      get #x
  , TestCase $ assertEqual "get(x) + get(y)" (Number 42) $ eval $
      withState #x (c 10) $
      withState #y (c 32) $
      get #x +. get #y
  , TestCase $ assertEqual "many get and put" (Number 32) $ eval $
      withState #x (c 10) $
      withState #y (c 11) $
      (#z =. get #x +. get #y) $
      put #x #z $$
      get #x +. get #y
  , TestCase $ assertEqual "nondet" (Number 44) $ eval $
      withStdLib $
      #sum :@
        withHandler
          (#x --> #cons :@ #x :@ v #nil)
          [(#choice, #_, #k) --> #concat :@ (#k :@ c 1) :@ (#k :@ c 10)]
          #nondet
        (do' #nondet #choice +. do' #nondet #choice)
  , TestCase $ assertEqual "pair" (Number 11) $ eval $
      (#tmp =. Pair (c 1) (c 10)) $
      Fst #tmp +. Snd #tmp
  , TestCase $ assertEqual "catch" (Number 42) $ eval catchExample
  , TestCase $ assertEqual "catch" (Number 42) $ eval catchExample2
  , TestCase $ assertEqual "embed" (Number 2) $ eval embedTest
  , TestCase $ assertEqual "embed" (Number 1) $ eval embedTest2
  , TestCase $ assertEqual "embed" (Number 5) $ eval embedTest3
  , TestCase $ assertEqual "embed" (Number 2) $ eval embedTest4
  , TestCase $ assertEqual "embed" (Number 3) $ eval embedTest6
  , TestCase $ assertEqual "transactional catch" (Number 42) $ eval transCatchFix
  ,  TestCase $ assertEqual "transactional catch" (Number 42) $ eval transCatchNoFix
  ,  TestCase $ assertEqual "transactional catch" (Number 42) $ eval transCatchNoFix2
  ,  TestCase $ assertEqual "concurrent actors" (Number 2) $ eval concurrentExample
  ,  TestCase $ assertEqual "concurrent global" (Number 3) $ eval concurrentExample2
  ]
