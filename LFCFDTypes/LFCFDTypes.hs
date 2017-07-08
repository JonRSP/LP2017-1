module LFCFDTypes where

type Id = String

type Gamma= [(Id, Tipo)]

data Tipo = TInt | TBool | TFuncao Tipo Tipo
 deriving(Show, Eq)

data Expressao = ValorI Int --ok
               | ValorB Bool --ok
               | Soma Expressao Expressao --ok
               | Subtracao Expressao Expressao --ok
               | Multiplicacao Expressao Expressao --ok
               | Divisao Expressao Expressao --ok
               | Let Id Expressao Expressao --ok
               | Ref Id  --ok
               | Lambda (Id, Tipo) Tipo Expressao --ok
               | Aplicacao Expressao Expressao
               | If Expressao Expressao Expressao --ok
 deriving(Show, Eq)

verificarTipos :: Expressao -> Gamma -> Maybe Tipo
verificarTipos (ValorI n) _   = return TInt --ok
verificarTipos (ValorB b) _   = return TBool --ok
verificarTipos (Lambda (v, t1) t2 exp) g = return (TFuncao t1 t2) --ok

--conferir se c é bool
verificarTipos (If c t e) gamma  = --ok
  verificarTipos c gamma >>= \lc ->
  if lc == TBool
    then verificarTipos t gamma >>= \lt ->
         verificarTipos e gamma >>= \re ->
         if lt == re then return lt else Nothing
    else Nothing

verificarTipos (Soma l r) gamma  = --ok
  verificarTipos l gamma >>= \lt ->
  verificarTipos r gamma >>= \rt ->
  if lt == TInt && rt == TInt then return TInt else Nothing

verificarTipos (Subtracao l r) gamma  = --ok
  verificarTipos l gamma >>= \lt ->
  verificarTipos r gamma >>= \rt ->
  if lt == TInt && rt == TInt then return TInt else Nothing

verificarTipos (Multiplicacao l r) gamma  = --ok
  verificarTipos l gamma >>= \lt ->
  verificarTipos r gamma >>= \rt ->
  if lt == TInt && rt == TInt then return TInt else Nothing

verificarTipos (Divisao l r) gamma  = --ok
  if r == (ValorI 0)
    then Nothing
    else
      verificarTipos l gamma >>= \lt ->
      verificarTipos r gamma >>= \rt ->
      if lt == TInt && rt == TInt then return TInt else Nothing

--mudar o let
verificarTipos (Let v e c) gamma = --ok
  verificarTipos e gamma >>= \t ->
  verificarTipos c ((v, t):gamma)

verificarTipos (Ref var) gamma = pesquisar var gamma --ok

verificarTipos (Aplicacao n a)      gamma = --ok
    verificarTipos n gamma >>= \t ->
    case t of
        (TFuncao t1 t2) -> verificarTipos a gamma >>= \arg ->
            if arg == t2
                then return arg
                else Nothing
        otherwise -> error ("Aplicacao de funcao nao anonima")


pesquisar :: Id -> Gamma -> Maybe Tipo --ok
pesquisar v [] = error "Variavel nao declarada."
pesquisar v ((i,e):xs)
 | v == i = Just e -- return e
 | otherwise = pesquisar v xs
