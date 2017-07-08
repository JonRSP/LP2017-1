verificarTipos(_, valor_inteiro(X), vInt) :- integer(X).

verificarTipos(_, valor_booleano(true), vBool) :- !.
verificarTipos(_, valor_booleano(false), vBool) :- !.
verificarTipos(_, valor_booleano(_), erro_tipo).
%lambda
verificarTipos(_,lambda((_,T1), T2, _),tFuncao(T1,T2)).


%let
verificarTipos(Gamma, let(X, Exp1, Exp2), Res) :-
  verificarTipos(Gamma, Exp1, Tn),
  verificarTipos([(X, Tn)|Gamma], Exp2, Res).

%soma
verificarTipos(Gamma, soma(LHS, RHS), Res) :-
    verificarTipos(Gamma, LHS, vInt),
    verificarTipos(Gamma, RHS, vInt),!,
    Res = vInt.
verificarTipos(_, soma(_, _), erro_tipo).

%subtracao
verificarTipos(Gamma, subtracao(LHS, RHS), Res) :-
    verificarTipos(Gamma, LHS, vInt),
    verificarTipos(Gamma, RHS, vInt),!,
    Res = vInt.
verificarTipos(_, subtracao(_, _), erro_tipo).

%multiplicacao
verificarTipos(Gamma, multiplicacao(LHS, RHS), Res) :-
    verificarTipos(Gamma, LHS, vInt),
    verificarTipos(Gamma, RHS, vInt),!,
    Res = vInt.
verificarTipos(_, multiplicacao(_, _), erro_tipo).

%divisao
verificarTipos(Gamma, divisao(LHS, RHS), Res) :-
    verificarTipos(Gamma, LHS, vInt),
    verificarTipos(Gamma, RHS, vInt),!,
    Res = vInt.
verificarTipos(_, divisao(_, _), erro_tipo).

%var
verificarTipos(Gamma, var(Var), Res) :-
    pesquisarAmbiente(var(Var), Gamma, Res).

%aplicacao
verificarTipos(Gamma, aplicacao(Exp1, Exp2), Res) :-
    verificarTipos(Gamma, Exp1, tFuncao(Targ, Texp)),!,
    verificarTipos(Gamma, Exp2, Texp),
    Texp == Targ, !,
    Res = Texp.
verificarTipos(_, aplicacao(_, _), aplicacao_requer_exp_lambda).
verificarTipos(_,_,erro_tipo).


pesquisarAmbiente(_, [], variavel_nao_encontrada) :- !.
pesquisarAmbiente(Var, [(Var, Exp)|_], Exp) :- !.
pesquisarAmbiente(Var, [_|Tail], Res) :- pesquisarAmbiente(Var, Tail, Res).
