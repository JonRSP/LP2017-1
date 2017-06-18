module LFCFDLazyTestes where

import LFCFDLazy

import Test.HUnit

v5 = Valor 5

let1 = Let "x" (Valor 5) (Lambda "y" (Soma (Ref "x") (Ref "y")))

aplicacao = Let "x" (Valor 5) (Aplicacao (Lambda "y" (Soma (Ref "x") (Ref "y"))) (Valor 3))

lambda01 = Lambda "x" (Soma (Ref "x") (Valor 1))
aplicarLambda01 = Aplicacao lambda01 (Valor 4)
teste10 = TestCase (assertEqual "avaliar ((\\x -> x+1)4)" (VInt 5) (avaliar aplicarLambda01 []))

let01 = Let "x" (Valor 5) (Soma (Ref "x") (Ref "x"))
teste4 = TestCase (assertEqual "avaliar let x = 5 in x + x" (VInt 10) (avaliar let01 []))

teste1 = TestCase (assertEqual "avaliar 5" (VInt 5) (avaliar v5 []))

teste2 = TestCase (assertEqual "avaliar Let x = 5 in (\\y -> x + y) 3" (VInt 8) (avaliar aplicacao []))

todosOsTestes = TestList [ teste2
                        -- , teste2
                         ]

executarTestes = runTestTT todosOsTestes
