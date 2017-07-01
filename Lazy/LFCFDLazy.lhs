\documentclass[12pt]{article}

%include polycode.fmt

\usepackage{busproofs}
\usepackage[brazil]{babel}

\title{Implementa\c c\~{a}o de Lazy Evaluation e Recurs\~ao em Haskell}

\author{H\'elio Santana da Silva J\'unior - 140142959 \\ J\^onatas Ribeiro Senna Pires - 140090983}


\begin{document}

\maketitle

\section{Introdu\c c\~{a}o}

Este documento apresenta uma implementa\c c\~{a}o, em \emph{literate Haskell}, de uma linguagem de programa\c c\~{a}o funcional minimalista utilizando a estrat\'egia de avali\c c\~{a}o lazy evaluation (sharing), que posterga as avalia\c c\~{o}es de express\~oes at\'e que elas sejam necess\'arias. O documento tamb\'em apresenta uma nova vers\~ao desta linguagem com suporte a recurss\~ao.

\section{Vis\~{a}o geral da linguagem}


A linguagem LFCFD suporta tanto express\~{o}es
identificadas (LET) quanto identificadores e fun\c c\~{o}es de alta ordem
(com o mecanismo de express\~oes lambda) al\'em de express\~oes recursivas e um condicional simples. A linguagem tamb\'em possui suporte para escopo est\'atico.

\section{Defini\c c\~{a}o da \'{A}rvore Sint\'{a}tica Abstrata}

A implementa\c c\~ao se baseia na defini\c c\~ao de um m\'odulo Haskell e alguns tipos auxiliares, como \texttt{Id} (um tipo sinomimo para uma string) e Env, que corresponde a um ambiente de valores, onde um valor pode ser um valor inteiro ou uma express\~ao lambda.

\begin{code}
module LFCFDLazy where

type Id = String

type Env = [(Id, ValorE)]

\end{code}

Existem duas defini\c c\~oes de tipos, \texttt{ValorE} e \texttt{Expressao}, onde o \texttt{ValorE} pode ser definido como um valor inteiro, um identificador com uma express\~ao (lambda) ou uma express\~ao simples. As express\~ao s\~ao definidas como um valor inteiro, express\~oes bin\'arias (soma, subtra\c c\~ao, multiplica\c c\~ao e divis\~ao), express\~oes \texttt{Let}, aplica\c c\~oes de fun\c c\~oes, recurs\~ao e \texttt{IF0}.


\begin{code}

data ValorE = VInt Int
            | FClosure Id Expressao Env
            | EClosure Expressao Env
 deriving(Show, Eq)

data Expressao = Valor Int
               | Soma Expressao Expressao
               | Subtracao Expressao Expressao
               | Multiplicacao Expressao Expressao
               | Divisao Expressao Expressao
               | Let Id Expressao Expressao
               | Ref Id
               | Lambda Id Expressao
               | Aplicacao Expressao Expressao
               | Recursao Id Expressao Expressao
               | IF0 Expressao Expressao Expressao
 deriving(Show, Eq)

\end{code}

A fun\c c\~ao que avalia as express\~oes recebe uma express\~ao e um ambiente e retorna um valorE. A avalia\c c\~ao de express\~oes necessita retornar uma closure, levando em conta que a linguagem esta utlizando substitui\c c\~oes postergadas. O closure mapeia cada vari\'avel livre da fun\c c\~ao dentro do contexto da pr\'opria fun\c c\~ao.

\begin{code}
avaliar :: Expressao -> Env -> ValorE
\end{code}

Para o caso de um valor inteiro a avial\c c\~ao \'e trivial, o retorno \'e um VInt, definido pelo \texttt{ValorE}. No caso de uma refer\^encia, e utilizado a fun\c c\~ao pesquisar, que realiza uma pesquisa da vari\'avel de refer\^encia dentro do ambiente. A fun\c c\~ao pesquisar assim como as demais fun\c c\~oes utilizadas dentro da fun\c c\~ao avaliar ser\~ao explicadas posteriormente neste documento.

\begin{code}
avaliar (Valor n)            _ = VInt n
avaliar (Ref v)             env = pesquisar v env
\end{code}

Em rela\c c\~ao aos casos de express\~oes bin\'arias a avali\c c\~ao e muito semelhante em todos os casos. \'E usada a fun\c c\~ao \texttt{avaliarExpBin} que recebe duas express\~oes, um operador e o ambiente.

\begin{code}
avaliar (Soma e d)          env = avaliarExpBin e d (+) env
avaliar (Subtracao e d)     env = avaliarExpBin e d (-) env
avaliar (Multiplicacao e d) env = avaliarExpBin e d (*) env
avaliar (Divisao e d)       env = avaliarExpBin e d div env
\end{code}

A avali\c c\~ao de uma expressao \texttt{Let} \'e traduzida como uma aplica\c c\~ao lambda. A express\~ao lambda e convertida em uma closure com uma vari\'avel e uma express\~ao.

\begin{code}
avaliar (Let v e c)         env = avaliar (Aplicacao (Lambda v c) e) env
avaliar (Lambda a c)        env = FClosure a c env
\end{code}

O caso de uma aplica\c c\~ao \'e o primeiro caso mais complexo da avalia\c c\~ao. Primeiramente \'e realizada uma avalia\c c\~ao diferente no primeiro argumento da aplica\c c\~ao, onde se espera retornar um \texttt{valorE}. Essa avali\c c\~ao foi chamada de \texttt{avaliacaoStrict}, que ser\'a aprofundada neste documento. Em seguida o segundo argumento e passado como uma closure de apenas uma express\~ao e ambiente. Caso a \texttt{avaliacaoStrict} retorne uma \texttt{FClosure}, e retornado uma avali\c c\~ao do corpo da closure passando o \texttt{Id} e a \texttt{Eclosure} dentro do ambiente, caso contrario \'e retornado um erro de aplica\c c\~ao de fun\c c\~ao.

\begin{code}
avaliar (Aplicacao e1 e2)   env =
  let
    v = avaliacaoStrict (avaliar e1 env)
    e = EClosure e2 env
  in case v of --if v = (FC a c env')
     (FClosure a c env') -> avaliar c ((a, e):env')
     otherwise -> error "Tentando aplicar uma expressao que nao eh uma funcao anonima"
\end{code}


O caso de uma express\~ao \texttt{IF0} \'e simples. Caso a condi\c c\~ao seja igual a zero a primeira express\~ao, t, \'e avaliada, caso contr\'ario a segunda express\~ao, \texttt{e}, \'e avaliada. Esse tipo de express\~ao \'e usada para a constru\c c\~ao de express\~oes recursivas. A expressao recursiva \'e avaliada de forma similar a uma aplica\c c\~ao de fun\c c\~ao, levando em conta sua semelhan\c ca estrutural com a expressao \texttt{Let}. A diferen\c ca \'e a atualiza\c c\~ao do ambiente, que deve ser atualizado em cada "la\c co" da recurs\~ao.

\begin{code}
avaliar (IF0 condicao t e) env
   | avaliar condicao env == VInt 0 = avaliar t env
   | otherwise = avaliar e env

avaliar (Recursao identificador valor corpo) env =
 let
   v = avaliacaoStrict (avaliar valor env)
   e = EClosure corpo env
   newEnv = (lookupApp identificador v env)++env
 in case v of
   (FClosure a c env') -> avaliar c ((a,e):newEnv)
   otherwise -> error "Tentando aplicar uma expressao que nao eh uma funcao anonima"
\end{code}

Em rela\c c\~ao as fun\c c\~oes auxiliares usadas na avalica\c c\~ao, a primeira a ser abodada ser\'a a fun\c c\~ao \texttt{avaliacaoStrict}. Ela recebe um \texttt{valorE} e retorna outro \texttt{valorE}. O objetivo dessa fun\c c\~ao \'e reduzir o \texttt{Eclosure} a um \texttt{VInt} ou retornar apenas um \texttt{FClosure}.

\begin{code}
avaliacaoStrict :: ValorE -> ValorE
avaliacaoStrict (EClosure e env) = avaliacaoStrict (avaliar e env)
avaliacaoStrict e = e
\end{code}

Outra fun\c c\~ao utilizada na fun\c c\~ao de avali\c c\~ao \'e a fun\c c\~ao pesquisar. Essa fun\c c\~ao \'e usada na avalica\c c\~ao de uma refer\^encia e ela recebe um \texttt{Id} e um ambiente e retorna um \texttt{valorE}. Ela procura o \texttt{Id} dentro do ambeiente e retorna a \texttt{avaliacaoStrict} da expressao referente a esse \texttt{Id}.

\begin{code}
pesquisar :: Id -> Env -> ValorE
pesquisar v [] = error "Variavel nao declarada."
pesquisar v ((i,e):xs)
 | v == i = avaliacaoStrict (e)
 | otherwise = pesquisar v xs
\end{code}

A fun\c c\~ao \texttt{lookupApp} \'e utilizada na avali\c c\~ao de uma express\~ao recursiva e ela \'e respons\'avel pela atualiza\c c\~ao do ambiente no decorrer da avalica\c c\~ao da recurss\~ao. Caso seja passado um ambiente vazio, a fun\c c\~ao ir\'a retornar uma lista com o idenditifcador e o valor passados como par\^ametro. Caso o identificador seja igual ao \texttt{Id} da cabe\c ca do ambiente, a fun\c c\~ao vai retornar uma lista vazia.

\begin{code}
lookupApp :: Id -> ValorE -> Env -> Env
lookupApp identificador valor [] = [(identificador, valor)]
lookupApp identificador valor ((i,e):xs)
 | identificador == i = []
 | otherwise = lookupApp identificador valor xs
\end{code}

A \'ultima fun\c c\~ao utilizada \'e a fun\c c\~ao de avalia\c c\~ao de uma express\~ao bin\'aria, que avalia cada lado da express\~ao levando em considera\c c\~ao seus operadores.

\begin{code}
avaliarExpBin :: Expressao -> Expressao -> (Int -> Int -> Int) -> Env -> ValorE
avaliarExpBin e d op env = VInt (op ve vd)
 where
  (VInt ve)  = avaliacaoStrict (avaliar e env)
  (VInt vd) = avaliacaoStrict (avaliar d env)
\end{code}


\section{Conclusao}

O presente trabalho consistiu no desenvolvimento de uma linguagem m\'inima no intuito de abordar conceitos como Lazy evaluation (sharing) e express\~oes recursivas. Foi mostrado que a linguagem tamb\'em dava suporte para fun\c c\~oes lambdas, expressoes \texttt{Let} e \texttt{IF0}.

\end{document}
