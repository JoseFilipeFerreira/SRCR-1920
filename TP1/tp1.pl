:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- op(900,xfy,'::').

:- dynamic '-'/1.
:- dynamic excecao/1.
:- dynamic adjudicante/3.
:- dynamic adjudicataria/3.
:- dynamic contrato/12.

:- include('invariantes.pl').
:- include('conhecimento.pl').
:- include('evolucao.pl').

% Representacao do tipo de contrato
tipoprocedimento(ad).
tipoprocedimento(cprev).
tipoprocedimento(cpub).

% Validar a data
datavalida(D, M, A) :-
    D > 0, D =< 31,
    M > 0, M =< 12,
    A > 0.

% Definicoes de negacoes fortes (nao verdadeiro e nao desconhecido)
-adjudicante(Nif, Nome, Morada) :-
    not(adjudicante(Nif, Nome, Morada)),
    not(excecao(adjudicante(Nif, Nome, Morada))).

-adjudicataria(Nif, Nome, Morada) :-
    not(adjudicataria(Nif, Nome, Morada)),
    not(excecao(adjudicataria(Nif, Nome, Morada))).

-contrato(Id, Ad, Ada, TContrato, TProcedimento, Descricao, Custo, Prazo, Local, Dia, Mes, Ano) :-
    not(contrato(Id, Ad, Ada, TContrato, TProcedimento, Descricao, Custo, Prazo, Local, Dia, Mes, Ano)),
    not(excecao(contrato(Id, Ad, Ada, TContrato, TProcedimento, Descricao, Custo, Prazo, Local, Dia, Mes, Ano))).

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao, falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    not( Questao ),
    not( -Questao ).

sum([], S) :- S is 0.
sum([H|T], S) :-
    sum(T, Si),
    S is Si + H.

not(Q):- Q, !, fail.
not(_).
