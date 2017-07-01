module LFCFDTypesTestes where

import LFCFDTypes

import Test.HUnit

v5 = ValorI 5
vT = ValorB True

teste1 = TestCase (assertEqual "verificarTipos 5" (Just TInt) (verificarTipos v5 []))

--teste2 = TestCase (assertEqual "verificarTipos True" (Just TBool) (verificarTipos vT []))

teste2 = TestCase (assertEqual "verificarTipos Soma(5 5)") (Just TInt) (verificarTipos (Soma (ValorI 5) (ValorI 5)) [])
--verificarTipos (Soma (ValorI 3) (ValorI 3)) []

todosOsTestes = TestList [ teste1
                          ,teste2

                         ]

executarTestes = runTestTT todosOsTestes
