:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- op(900,xfy,'::').

:- dynamic '-'/1.
:- dynamic excecao/1.
:- dynamic adjudicante/3.
:- dynamic adjudicataria/3.
:- dynamic contrato/12.

% Conhecimento Perfeito Positivo
adjudicataria(500000000, sarilhos, braga).
adjudicataria(500000001, limpalimpinho, porto).
adjudicataria(500000002, oficina, lisboa).

adjudicante(600000000, bdp, lisboa).
adjudicante(600000001, chmtad, vilareal).

contrato(0, 600000000, 500000000, "Aquisicao de servicos", ad, "Assessoria juridica", 4000, 100, porto, 10, 10, 2010).
contrato(1, 600000003, 500000000, "Aquisicao de servicos", cprev, "Assessoria juridica", 1000000, 346, porto, 10, 10, 2010).

% Conhecimento Perfeito Negativo
% Nao existe um adjudicante com o Nif 600000010, com o Nome "bombeiros" e sede em evora
-adjudicante(600000010, bombeiros, evora).

% Conhecimento Imperfeito Incerto
% Nao se sabe o nome da adjudicataria com o nif 500000003, com sede em satantarem
adjudicataria(500000003, incerto, santarem).
excecao(adjudicataria(Nif, _, Morada)) :-
    adjudicataria(Nif, incerto, Morada).
incertoNome(adjudicataria(500000003), incerto).

% Nao se sabe o valor do contrato celebrado com o ID 1
contrato(2, 600000001, 500000000, "Aquisicao de servicos", cprev, "Assessoria juridica", incerto, 520, porto, 10, 10, 2010).
excecao(contrato(Id, Ad, Ada, TContrato, TProcedimento, Descricao, _, Prazo, Local, Dia, Mes, Ano)) :-
    contrato(Id, Ad, Ada, TContrato, TProcedimento, Descricao, incerto, Prazo, Local, Dia, Mes, Ano).
incertoValor(contrato(2), incerto).

% Conhecimento Imperfeito Impreciso
% Nao sabemos se o Nif corresponde ao exercito ou aos bombeiros
excecao(adjudicante(600000005, exercito, lisboa)).
excecao(adjudicante(600000005, bombeiros, lisboa)).
impreciso(adjudicante(600000005)).

% Nao sabemos se o contrato com o ID 3 foi para Aquisicao de servicos ou de bens moveis
excecao(contrato(3, 600000001, 500000002, "Aquisicao de servicos", cprev, "Assessoria juridica", 1000000, 346, porto, 10, 10, 2010)).
excecao(contrato(3, 600000001, 500000002, "Aquisicao de bens moveis", cprev, "Assessoria juridica", 1000000, 346, porto, 10, 10, 2010)).
impreciso(contrato(3)).

% Nao sabemos o valor do contrato 4, mas sabemos que que esta dentro dos valores legais para contratos de ajustes diretos
excecao(contrato(4, 600000001, 500000002, "Aquisicao de servicos", ad, "Assessoria juridica", Valor, 346, porto, 10, 10, 2010)) :-
    Valor >= 0,
    Valor =< 5000.
impreciso(contrato(4)).

% Conhecimento Imperfeito Interdito
% Nunca sera possivel saber com quem a adjudicataria do contrato com o ID 5
contrato(5, 600000001, interdito, "Aquisicao de servicos", cprev, "Assessoria juridica", 2000, 520, porto, 10, 10, 2010).
excecao(contrato(Id, Ad, _, TContrato, TProcedimento, Descricao, Custo, Prazo, Local, Dia, Mes, Ano)) :-
    contrato(Id, Ad, interdito, TContrato, TProcedimento, Descricao, Custo, Prazo, Local, Dia, Mes, Ano).
interditoAda(contrato(5), interdito).
+contrato(_, _, _, _, _, _, _, _, _, _, _, _) :: (findall(Ada, (contrato(5, _, Ada, _, _, _, _, _, _, _, _, _), not(interditoAda(contrato(5), Ada))), S), 
                                                    length(S, N),
                                                    N == 0).
 
% Nunca sera possivel saber o nome do adjudicante com nif 600000010
adjudicante(600000010, interdito, braga).
excecao(adjudicante(Nif, _, Morada)) :-
    adjudicante(Nif, interdito, Morada).
interditoNome(adjudicante(600000010), interdito).
+adjudicante(_, _, _) :: (findall(L, (adjudicante(600000010, L, _), not(interditoNome(adjudicante(600000010), L))), S), length(S, 0)).
