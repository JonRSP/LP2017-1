\documentclass[12pt]{article}

%include polycode.fmt

\usepackage{busproofs}
\usepackage[brazil]{babel}
\usepackage[utf8]{inputenc}

\title{Implementa\c c\~{a}o de Verifica\c c\~{a}o de Tipos em Haskell}

\author{H\'elio Santana da Silva J\'unior - 140142959 \\ J\^onatas Ribeiro Senna Pires - 140090983}

\begin{document}

\maketitle

\section{Introdu\c c\~{a}o}

Esse documento apresenta uma implementa\c c\~{a}o,
em \emph{literate Haskell}, do mecanismo de verifica\c c\~{a}o
de tipos de uma linguagem de programa\c c\~{a}o funcional
minimalista. Esta versão implementa a verificação de tipos para todos os elementos sintaticos, como aplicação de função, expressões lambda, let, if e expressões binarias.

\section{Vis\~{a}o geral da linguagem}


A linguagem LFCF suporta tanto express\~{o}es
identificadas (LET) quanto identificadores e fun\c c\~{o}es de alta ordem
(com o mecanismo de expressoes lambda). O foco \'{e} na
verifica\c c\~{a}o de tipos, ent\~{a}o n\~{a}o est\~{a}o
implementadas fun\c c\~{o}es voltadas para a avalia\c c\~{a}o
das express\~{o}es.

\section{Defini\c c\~{a}o da \'{A}rvore Sint\'{a}tica Abstrata}

A implementa\c c\~{a}o consiste na defini\c c\~{a}o de
um m\'{o}dulo Haskell mais alguns tipos auxiliares, como
\texttt{Id} (um tipo sin\^{o}nimo para uma \texttt{string}) e
\texttt{Gamma}, que corresponde a um mapeamento de
identificadores em tipos.

\begin{code}
module LFCFDTypes where

type Id = String

type Gamma= [(Id, Tipo)]

\end{code}

Os tipos v\'{a}lidos s\~{a}o definidos com o
tipo alg\'{e}brico \texttt{Tipo}, que pode
ser um tipo inteiro, um tipo booleano
e um tipo fun\c c\~{a}o. O tipo fun\c c\~{a}o
deve expressar tanto o tipo do argumento quanto
o tipo do retorno. As express\~{o}es, conforme
mencionado anteriormente, envolvem tanto
valores inteiros quanto booleanos, bem
como express\~{o}es bin\'{a}rias (soma,
subtra\c c\~{a}o, etc.), express\~{o}es
\texttt{let}, \texttt{lambda}, aplica\c c\~{a}o
de fun\c c\~{o}es e \texttt{if-then-else}

\begin{code}

data Tipo = TInt | TBool | TFuncao Tipo Tipo
 deriving(Show, Eq)

data Expressao = ValorI Int
               | ValorB Bool
               | Soma Expressao Expressao
               | Subtracao Expressao Expressao
               | Multiplicacao Expressao Expressao
               | Divisao Expressao Expressao
               | Let Id Expressao Expressao
               | Ref Id
               | Lambda (Id, Tipo) Tipo Expressao
               | Aplicacao Expressao Expressao
               | If Expressao Expressao Expressao
 deriving(Show, Eq)

\end{code}

A fun\c c\~{a}o que realiza a verifica\c c\~{a}o
de tipos recebe uma express\~{a}o, um ambiente
\texttt{Gamma} e possivelmente retorna um tipo
v\'{a}lido (por isso o retorno \texttt{Maybe Tipo}).
Caso algum erro ocorra no sistema de tipos,
essa fun\c c\~{a}o deve retornar \texttt{Nothing}.
Isso permite o uso de uma nota\c c\~{a}o
baseada em monadas.

\begin{code}
verificarTipos :: Expressao -> Gamma -> Maybe Tipo
\end{code}

Para alguns casos, a verifica\c c\~{a}o de tipos
\'{e} bem trivial, particularmente a verifica\c c\~{a}o
de tipos de express\~{o}es envolvendo valores inteiros,
valores booleanos e express\~{o}es \texttt{lambda}

\begin{code}
verificarTipos (ValorI n) _   = return TInt

verificarTipos (ValorB b) _   = return TBool

verificarTipos (Lambda (v, t1) t2 exp) g = return (TFuncao t1 t2)
\end{code}

Para outros casos, a verifica\c c\~{a}o de tipos
requer um certo grau de indu\c c\~{a}o (seguindo as
regras de deriva\c c\~{a}o vistas em sala de aula). Para
a soma, temos a seguinte regra de deriva\c c\~{a}o:

\begin{prooftree}
\AxiomC{$\Gamma\vdash lhs : TInt$}
\AxiomC{$\Gamma\vdash rhs : TInt$}
\BinaryInfC{$\Gamma\vdash soma(lhs, rhs) : TInt$}
\end{prooftree}

\noindent que pode ser traduzida para Haskell como:

\begin{code}
verificarTipos (Soma l r) gamma  =
  verificarTipos l gamma >>= \lt ->
  verificarTipos r gamma >>= \rt ->
  if lt == TInt && rt == TInt then return TInt else Nothing
\end{code}

Pode-se notar que a verificação do tipo das outras expressões binarias, como multiplicação, divisão e subtração segue a mesma regra de derivação. Apenas no caso da divisão que a implementação tem uma leve mudança, pois caso o denominador seja zero, a função deve retornar o tipo Nothing, pois divisões por zero são tratados como erro.

\begin{code}
verificarTipos (Subtracao l r) gamma  =
  verificarTipos l gamma >>= \lt ->
  verificarTipos r gamma >>= \rt ->
  if lt == TInt && rt == TInt then return TInt else Nothing

verificarTipos (Multiplicacao l r) gamma  =
  verificarTipos l gamma >>= \lt ->
  verificarTipos r gamma >>= \rt ->
  if lt == TInt && rt == TInt then return TInt else Nothing

verificarTipos (Divisao l r) gamma  =
  if r == (ValorI 0)
    then Nothing
    else
      verificarTipos l gamma >>= \lt ->
      verificarTipos r gamma >>= \rt ->
      if lt == TInt && rt == TInt then return TInt else Nothing

\end{code}


Similarmente, a verifica\c c\~{a}o de express\~{o}es do tipo
\texttt{let} requer um grau de indu\c c\~{a}o. Supondo
uma express\~{a}o \texttt{let v = e in c}, primeiro verificamos
o tipo da express\~{a}o nomeda (\texttt{e}) \'{e} bem tipada com
tipo \texttt{t},
adicionamos uma associa\c c\~{a}o \texttt{(v, t)} no
ambiente \texttt{Gamma} original e computamos o tipo de
\texttt{c} no novo ambiente. Em termos de regras de deriva\c c\~{a}o,
ter\'{a}mos:

\begin{prooftree}
\AxiomC{$\Gamma\vdash e : \tau_1$}
\AxiomC{$(x,\tau_1)\Gamma\vdash c : \tau_2$}
\BinaryInfC{$\Gamma\vdash let(v,e,c) : \tau_2$}
\end{prooftree}

\noindent Em Haskell:

\begin{code}
verificarTipos (Let v e c) gamma =
  verificarTipos e gamma >>= \t ->
  verificarTipos c ((v, t):gamma)

\end{code}

Em relaçao a referencias de variaveis, a implementação é relativamente simples. Deve-se primeiro pesquisar se a variavel está declarada no ambiente, caso dela não esteja, é retornado um erro de variavel não encontrada, caso a variavel esta declarada no ambiente é retornado o tipo associado a essa variavel.

\begin{code}
verificarTipos (Ref var) gamma = pesquisar var gamma

pesquisar :: Id -> Gamma -> Maybe Tipo
pesquisar v [] = error "Variavel nao declarada."
pesquisar v ((i,e):xs)
 | v == i = Just e -- return e
 | otherwise = pesquisar v xs

\end{code}

Na verificação dos tipos de expressões if a primeira coisa que deve ser analisada é a condição. Para que a expressão seja valida a condição deve ser do tipo booleano. Em seguida, o tipo da expressão deve ser ser determinada a partir dos tipos das clausulas then e else. Caso os tipos sejam iguais, este será o tipo da expressão como um todo. Caso eles sejam diferentes, a função deve retornar um erro de tipo. A regra para definição do tipo segue:

\begin{prooftree}
    \AxiomC{$\Gamma\vdash condicao : \texttt{boolean}$}
    \AxiomC{$\Gamma\vdash then : \tau$}
    \AxiomC{$\Gamma\vdash else : \tau$}
    \TrinaryInfC{$\Gamma\vdash \{\texttt{if}$ $condicao$ $then$ $else\} : \tau$}
\end{prooftree}

\noindent Em Haskell:

\begin{code}
verificarTipos (If c t e) gamma  =
  verificarTipos c gamma >>= \lc ->
  if lc == TBool
    then verificarTipos t gamma >>= \lt ->
         verificarTipos e gamma >>= \re ->
         if lt == re then return lt else Nothing
    else Nothing
\end{code}

Por fim, temos a verfificação da expressão aplicação de função. Esta verificação possui alguns passos, primeiramente deve-se analisar se a definição da função é um expressão lambda, onde toda expressão lambda possui o tipo TFuncao tId tExp. Em seguida deve-se verificar o tipo do argumento da aplicação de função. Por ultimo é feito uma comparação entre o tipo do argumento da função com o tipo tExp. Caso esses tipos forem iguais, o tipo da aplicação será o tipo do argumento. Caso sejam diferentes, a função retornará o tipo Nothing.

\noindent A prova deste caso é definida assim:

\begin{prooftree}
    \AxiomC{$\Gamma\vdash definicao : (\texttt{TFuncao}$ $\tau_{1}$ $\tau_{2})$}
    \AxiomC{$\Gamma\vdash argumento : \tau_{1}$}
    \BinaryInfC{$\Gamma\vdash \{$\texttt{Aplicacao }$definicao$ $argumento\} : \tau_{2}$}
\end{prooftree}

\noindent Em haskell:

\begin{code}
verificarTipos (Aplicacao n a)      gamma =
    verificarTipos n gamma >>= \t ->
    case t of
        (TFuncao t1 t2) -> verificarTipos a gamma >>= \arg ->
            if arg == t2
                then return arg
                else Nothing
        otherwise -> error ("Aplicacao de funcao nao anonima")
\end{code}

\section{Conclusão}

O presente trabalho mostrou a implementação de um verificador simples e tipos em haskell, para uma linguagem que dá suporte para expressões lambda, let, if, aplicações de função e expressões binarias.
\end{document}
