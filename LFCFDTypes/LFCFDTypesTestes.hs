module LFCFDTypesTestes where

import LFCFDTypes

import Test.HUnit


v5 = ValorI 5
v4 = ValorI 4
v3 = ValorI 3
v2 = ValorI 2
v1 = ValorI 1
v0 = ValorI 0

vt = ValorB True
vf = ValorB False

amb = [("x", TInt), ("z", TBool), ("y", TInt)]

verSoma = (Soma (v2) (v5))

verSub = (Subtracao (v2) (v5))

verMult = (Multiplicacao (v2) (v5))

verDiv = (Divisao (v2) (v5))

verSoma1 = (Soma (v2) (Ref "x"))

verSub1 = (Subtracao (v2) (Ref "x"))

verMult1 = (Multiplicacao (v2) (Ref "x"))

verDiv1 = (Divisao (v2) (Ref "x"))

verDiv2 = (Divisao (v3) (v0))


let1 = Let "x" (ValorI 4) (Soma (Ref "x")(Ref "x"))

let2 = Let "x" (ValorI 4) (Soma (Ref "x")(v2))

let3 = Let "x" (ValorB True) (Ref "x")

let4 = Let "x" (ValorB True) (Soma (ValorI 2) (ValorI 3))

let5 = Let "x" (ValorB True) (Soma (Ref "x") (ValorI 3))



teste = TestCase (assertEqual "verificarTipos 5" (Just TInt) (verificarTipos v5 []))

teste1 = TestCase (assertEqual "verificarTipos True" (Just TBool) (verificarTipos vt []))

teste2 = TestCase (assertEqual "verificarTipos let x = 5 in x + 2" (Just TInt) (verificarTipos let2 []))

teste3 = TestCase (assertEqual "verificarTipos let x = 5 in x + x" (Just TInt) (verificarTipos let1 amb))

teste4 = TestCase (assertEqual "verificarTipos let x = True in x" (Just TBool) (verificarTipos let3 []))

teste5 = TestCase (assertEqual "verificarTipos x + 2" (Just TInt) (verificarTipos verSoma1 amb))

teste6 = TestCase (assertEqual "verificarTipos x - 2" (Just TInt) (verificarTipos verSub1 amb))

teste7 = TestCase (assertEqual "verificarTipos x / 2" (Just TInt) (verificarTipos verDiv1 amb))

teste8 = TestCase (assertEqual "verificarTipos x * 2" (Just TInt) (verificarTipos verMult1 amb))

teste9 = TestCase (assertEqual "verificarTipos 5 + 2" (Just TInt) (verificarTipos verSoma []))

teste10 = TestCase (assertEqual "verificarTipos 5 - 2" (Just TInt) (verificarTipos verSub []))

teste11 = TestCase (assertEqual "verificarTipos 5 / 2" (Just TInt) (verificarTipos verDiv []))

teste12 = TestCase (assertEqual "verificarTipos 5 * 2" (Just TInt) (verificarTipos verMult []))

teste13 = TestCase (assertEqual "verificarTipos 3 / 0" (Nothing) (verificarTipos verDiv2 []))

teste14 = TestCase (assertEqual "verificarTipos if true then true else false" (Just TBool) (verificarTipos (If (vt) (vt) (vf)) []))

teste15 = TestCase (assertEqual "verificarTipos if false then true else false" (Just TBool) (verificarTipos (If (vf) (vt) (vf)) []))

teste16 = TestCase (assertEqual "verificarTipos if true then 5 else 10" (Just TInt) (verificarTipos (If (vt) (ValorI 5) (ValorI 10)) []))

teste17 = TestCase (assertEqual "verificarTipos if 2 then true else false" (Nothing) (verificarTipos (If (ValorI 2) (vt) (vf)) []))

teste18 = TestCase (assertEqual "verificarTipos if 2 then 3 else 4" (Nothing) (verificarTipos (If (ValorI 2) (v3) (v4)) []))

teste19 = TestCase (assertEqual "verificarTipos if true then 2 else false" (Nothing) (verificarTipos (If (vt) (v2) (vf)) []))

teste20 = TestCase (assertEqual "verificarTipos if true then true else 5" (Nothing) (verificarTipos (If (vt) (vt) (v5)) []))

todosOsTestes = TestList [ teste
                         , teste1
                         , teste2
                         , teste3
                         , teste4
                         , teste5
                         , teste6
                         , teste7
                         , teste8
                         , teste9
                         , teste10
                         , teste11
                         , teste12
                         , teste13
                         , teste14
                         , teste15
                         , teste16
                         , teste17
                         , teste18
                         , teste19
                         , teste20
                         ]

executarTestes = runTestTT todosOsTestes
