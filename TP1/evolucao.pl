:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- op(900,xfy,'::').

:- dynamic '-'/1.
:- dynamic excecao/1.
:- dynamic adjudicante/3.
:- dynamic adjudicataria/3.
:- dynamic contrato/12.
:- dynamic interditoNome/2.
:- dynamic interditoMorada/2.
:- dynamic impreciso/1.
:- dynamic incertoNome/2.
:- dynamic incertoMorada/2.
:- dynamic removeImpreciso/1.
:- dynamic removeIncerto/1.

% Evolucao do conhecimento
% Inserir novo conhecimento perfeito na base de conhecimento
evolucao(Termo) :-
    demo(Termo, falso),
    inserir(Termo).

% Atualizar conhecimento imperfeito na base de conhecimento
evolucao(adjudicante(Nif, Nome, Morada)) :-
    not(interditoNome(adjudicante(Nif), _)),
    not(interditoMorada(adjudicante(Nif), _)),
    removeIncerto(adjudicante(Nif)),
    inserir(adjudicante(Nif, Nome, Morada)).

evolucao(adjudicante(Nif, Nome, Morada)) :-
    not(interditoNome(adjudicante(Nif), _)),
    not(interditoMorada(adjudicante(Nif), _)),
    removeImpreciso(adjudicante(Nif)),
    inserir(adjudicante(Nif, Nome, Morada)).

evolucao(adjudicataria(Nif, Nome, Morada)) :-
    not(interditoNome(adjudicataria(Nif), _)),
    not(interditoMorada(adjudicataria(Nif), _)),
    removeIncerto(adjudicataria(Nif)),
    inserir(adjudicataria(Nif, Nome, Morada)).

evolucao(adjudicataria(Nif, Nome, Morada)) :-
    not(interditoNome(adjudicataria(Nif), _)),
    not(interditoMorada(adjudicataria(Nif), _)),
    removeImpreciso(adjudicataria(Nif)),
    inserir(adjudicataria(Nif, Nome, Morada)).

% Inserir conhecimento impreciso na base de conhecimento
% Evolucao das adjudicatarias
evolucaoImpreciso([adjudicataria(Nif, N, M) | R]) :-
	R \= [],
	mesmoNif(R, Nif),
	nenhumPerfeito([adjudicataria(Nif, N, M) | R]),
	insereExcecoes([adjudicataria(Nif, N, M) | R]).

mesmoNif([], _).
mesmoNif([adjudicataria(Nif, _, _) | R], Nif1) :-
	Nif == Nif1,
    mesmoNif(R, Nif1).

nenhumVerdadeiro([]).
nenhumVerdadeiro([X|Xs]) :-
	demo(X,falso),
	nenhumPerfeito(Xs).
nenhumVerdadeiro([X|Xs]) :-
	demo(X,desconhecido),
	nenhumPerfeito(Xs).

insereExcecoes([]).
insereExcecoes([adjudicataria(Nif, N, M) | R]) :-
	assert(excecao(adjudicataria(Nif, N, M))),
	insereExcecoes(R),
	assert(impreciso(adjudicataria(Nif))).

% Evolucao dos adjudicantes
evolucaoImpreciso([adjudicante(Nif, N, M) | R]) :-
	R \= [],
	mesmoNif(R, Nif),
	nenhumPerfeito([adjudicante(Nif, N, M) | R]),
	insereExcecoes([adjudicante(Nif, N, M) | R]).

mesmoNif([], _).
mesmoNif([adjudicante(Nif, _, _) | R], Nif1) :-
	Nif == Nif1,
    mesmoNif(R, Nif1).

nenhumPerfeito([]).
nenhumPerfeito([X|Xs]) :-
	demo(X,falso),
	nenhumPerfeito(Xs).
nenhumPerfeito([X|Xs]) :-
	demo(X,desconhecido),
	nenhumPerfeito(Xs).

insereExcecoes([]).
insereExcecoes([adjudicante(Nif, N, M) | R]) :-
	assert(excecao(adjudicante(Nif, N, M))),
	insereExcecoes(R),
	assert(impreciso(adjudicante(Nif))).

% Inserir conhecimento incerto na base de conhecimento 
% Adjudicataria
evolucaoIncertoNome(adjudicataria(Nif, N, M)) :-
    demo(adjudicataria(Nif, _, M), falso),
    not(interditoMorada(adjudicataria(Nif), _)),
    assert(excecao(adjudicataria(_, _, _)) :- adjudicataria(_, N, _)),
    inserir(adjudicataria(Nif, N, M)),
    assert(incertoNome(adjudicataria(Nif), N)).

evolucaoIncertoMorada(adjudicataria(Nif, N, M)) :-
    demo(adjudicataria(Nif, N, _), falso),
    not(interditoMorada(adjudicataria(Nif), _)),
    assert(excecao(adjudicataria(_, _, _)) :- adjudicataria(_, _, M)),
    inserir(adjudicataria(Nif, N, M)),
    assert(incertoMorada(adjudicataria(Nif), M)).

% Adjudicante
evolucaoIncertoNome(adjudicante(Nif, N, M)) :-
    demo(adjudicante(Nif, _, M), falso),
    not(interditoNome(adjudicante(Nif), _)),
    assert(excecao(adjudicante(_, _, _)) :- adjudicante(_, N, _)),
    inserir(adjudicante(Nif, N, M)),
    assert(incertoNome(adjudicante(Nif), N)).

evolucaoIncertoMorada(adjudicante(Nif, N, M)) :-
    demo(adjudicante(Nif, N, _), falso),
    not(interditoMorada(adjudicante(Nif), _)),
    assert(excecao(adjudicante(_, _, _)) :- adjudicante(_, _, M)),
    inserir(adjudicante(Nif, N, M)),
    assert(incertoMorada(adjudicante(Nif), M)).

% Inserir conhecimento interdito
evolucaoInterditoNome(adjudicante(Nif, Nome, Morada)) :-
    inserir(adjudicante(Nif, Nome, Morada)),
    assert(excecao(adjudicante(_, _, _)) :- adjudicante(_, Nome, _)),
    assert(interditoNome(adjudicante(Nif), Nome)),
    assert(+adjudicante(_, _, _) :: (findall(L, (adjudicante(Nif, L, _), not(interditoNome(adjudicante(Nif), L))), S), length(S, 0))).

evolucaoInterditoMorada(adjudicante(Nif, Nome, Morada)) :-
    inserir(adjudicante(Nif, Nome, Morada)),
    assert(excecao(adjudicante(_, _, _)) :- adjudicante(_, _, Morada)),
    assert(interditoMorada(adjudicante(Nif), Morada)),
    assert(+adjudicante(_, _, _) :: (findall(L, (adjudicante(Nif, _, L), not(interditoMorada(adjudicante(Nif), L))), S), length(S, 0))).

evolucaoInterditoNome(adjudicataria(Nif, Nome, Morada)) :-
    inserir(adjudicataria(Nif, Nome, Morada)),
    assert(excecao(adjudicataria(_, _, _)) :- adjudicataria(_, Nome, _)),
    assert(interditoNome(adjudicataria(Nif), Nome)),
    assert(+adjudicataria(_, _, _) :: (findall(L, (adjudicataria(Nif, L, _), not(interditoNome(adjudicataria(Nif), L))), S), length(S, 0))).

evolucaoInterditoMorada(adjudicataria(Nif, Nome, Morada)) :-
    inserir(adjudicataria(Nif, Nome, Morada)),
    assert(excecao(adjudicataria(_, _, _)) :- adjudicataria(_, _, Morada)),
    assert(interditoMorada(adjudicataria(Nif), Morada)),
    assert(+adjudicataria(_, _, _) :: (findall(L, (adjudicataria(Nif, _, L), not(interditoMorada(adjudicataria(Nif), L))), S), length(S, 0))).

% Remover conhecimento da base de conhecimento
regressao(T) :- 
    demo(T, verdadeiro),
    remover(T).

% Remover conhecimento incerto
regressaoIncerto(adjudicante(Nif)) :-
    demo(adjudicante(Nif, _, _), verdadeiro),
    findall(Invariante,-adjudicante(Nif, _, _)::Invariante,S),
    removeIncerto(adjudicante(Nif)),
    teste(S).

% Remover conhecimento incerto
regressaoIncerto(adjudicataria(Nif)) :-
    demo(adjudicataria(Nif, _, _), verdadeiro),
    findall(Invariante,-adjudicataria(Nif, _, _)::Invariante,S),
    removeIncerto(adjudicataria(Nif)),
    teste(S).

regressaoIncerto(adjudicante(Nif)) :-
    demo(adjudicante(Nif, _, _), verdadeiro),
    findall(Invariante,-adjudicante(Nif, _, _)::Invariante,S),
    removeIncerto(adjudicante(Nif)),
    teste(S).

% Remover conhecimento incerto
regressaoImpreciso(adjudicataria(Nif)) :-
    demo(excecao(adjudicataria(Nif, _, _)), verdadeiro),
    findall(Invariante,-excecao(adjudicataria(Nif, _, _))::Invariante,S),
    removeImpreciso(adjudicataria(Nif)),
    teste(S).

regressaoImpreciso(adjudicante(Nif)) :-
    demo(excecao(adjudicante(Nif, _, _)), verdadeiro),
    findall(Invariante,-excecao(adjudicante(Nif, _, _))::Invariante,S),
    removeImpreciso(adjudicante(Nif)),
    teste(S).


% Remover conhecimento imperfeito para atualizar com perfeito
removeIncerto(adjudicataria(Nif)) :-
    incertoNome(adjudicataria(Nif), I),
    recursiveRemove(excecao(adjudicataria(_, _, _)) :- adjudicataria(_, I, _)),
    recursiveRemove(adjudicataria(Nif, I, _)),
    remove(incertoNome(adjudicataria(Nif, I))).

removeIncerto(adjudicataria(Nif)) :-
    incertoMorada(adjudicataria(Nif), I),
    recursiveRemove(excecao(adjudicataria(_, _, _)) :- adjudicataria(_, _, I)),
    recursiveRemove(adjudicataria(Nif, _, I)),
    remove(incertoMorada(adjudicataria(Nif), I)).

removeIncerto(adjudicante(Nif)) :-
    incertoNome(adjudicante(Nif), I),
    recursiveRemove(excecao(adjudicante(_, _, _) :- adjudicante(_, I, _))),
    recursiveRemove(adjudicante(Nif, I, _)),
    remove(incertoNome(adjudicante(Nif), I)).

removeIncerto(adjudicante(Nif)) :-
    incertoMorada(adjudicante(Nif), I),
    recursiveRemove(excecao(adjudicante(_, _, _) :- adjudicante(_, _, I))),
    recursiveRemove(adjudicante(Nif, _, I)),
    remove(incertoMorada(adjudicante(Nif), I)).

removeImpreciso(adjudicataria(Nif)) :-
    impreciso(adjudicataria(Nif)),
    retract(excecao(adjudicataria(Nif, _, _))),
    removeImpreciso(adjudicataria(Nif)),
    retract(impreciso(adjudicataria(Nif))).

removeImpreciso(adjudicante(Nif)) :-
    impreciso(adjudicante(Nif)),
    retract(excecao(adjudicante(Nif, _, _))),
    removeImpreciso(adjudicante(Nif)),
    retract(impreciso(adjudicante(Nif))).

removeImpreciso(_).

% Predicados uteis
inserir(T) :-
    findall(Invariante, +T::Invariante, S),
    insert(T),
    teste(S).

remover(T) :-
    T,
    findall(Invariante,-T::Invariante,S),
    remove(T),
    teste(S).

removeExcecao([]).
removeExcecao([excecao(T) | X]) :-
    remove(excecao(T)),
    remove(T),
    removeExcecao(X).
removeExcecao([excecao(T) | X]) :-
    remove(excecao(T)),
    removeExcecao(X).

recursiveRemove(T) :-
    remove(T),
    T,
    recursiveRemove(T).
recursiveRemove(_).

remove(T) :-
    retract(T).
remove(T) :-
    assert(T), !, fail.

insert(T) :-
    assert(T).
insert(T) :-
    retract(T),!,fail.

teste([]).
teste([I|L]) :-
    I,
    teste(L).
