\documentclass[12pt]{article}

%include polycode.fmt

\usepackage{busproofs}
\usepackage[brazil]{babel}

\title{Implementa\c c\~{a}o de Verifica\c c\~{a}o de Tipos em Haskell}

\author{Rodrigo Bonif\'{a}cio}

\begin{document}

\maketitle

\section{Introdu\c c\~{a}o}

Esse documento apresenta uma implementa\c c\~{a}o,
em \emph{literate Haskell}, do mecanismo de verifica\c c\~{a}o
de tipos de uma linguagem de programa\c c\~{a}o funcional
minimalista. Os alunos da disciplina Linguagens de Programa\c c\~{a}o
devem extender essa implementa\c c\~{a}o de tal forma que
todos os elementos sint\'{a}ticos possuam a verifica\c c\~{a}o
de tipos implementada.

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

\section{Trabalho} 

Essa atividade do projeto final envolve verificar os tipos das demais expressoes
e escrever casos de teste para verificar se esta tudo ok. Para simplificar,
escolha a estrategia de escopo (dinamico ou estatico).



\end{document}
