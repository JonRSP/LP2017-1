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
verificarTipos (ValorI n) _   = return TInt
verificarTipos (ValorB b) _   = return TBool
verificarTipos (Lambda (v, t1) t2 exp) g = return (TFuncao t1 t2)

verificarTipos (If c t e) gamma  =
  verificarTipos t gamma >>= \lt ->
  verificarTipos e gamma >>= \re ->
  if lt == re then return lt else Nothing

verificarTipos (Soma l r) gamma  =
  verificarTipos l gamma >>= \lt ->
  verificarTipos r gamma >>= \rt ->
  if lt == TInt && rt == TInt then return TInt else Nothing

verificarTipos (Subtracao l r) gamma  =
  verificarTipos l gamma >>= \lt ->
  verificarTipos r gamma >>= \rt ->
  if lt == TInt && rt == TInt then return TInt else Nothing

verificarTipos (Multiplicacao l r) gamma  =
  verificarTipos l gamma >>= \lt ->
  verificarTipos r gamma >>= \rt ->
  if lt == TInt && rt == TInt then return TInt else Nothing

verificarTipos (Divisao l r) gamma  =
  verificarTipos l gamma >>= \lt ->
  verificarTipos r gamma >>= \rt ->
  if lt == TInt && rt == TInt then return TInt else Nothing

verificarTipos (Let v e c) gamma =
  verificarTipos e gamma >>= \t ->
  verificarTipos c ((v, t):gamma)

verificarTipos (Ref var) gamma = pesquisar var gamma

pesquisar :: Id -> Gamma -> Maybe Tipo
pesquisar v [] = error "Variavel nao declarada."
pesquisar v ((i,e):xs)
 | v == i = Just e
 | otherwise = pesquisar v xs
