:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- op(900,xfy,'::').

:- dynamic '-'/1.
:- dynamic excecao/1.
:- dynamic adjudicante/3.
:- dynamic adjudicataria/3.
:- dynamic contrato/12.


% Invariante que nao permite existir mais do que uma adjudicataria com o mesmo nif
+adjudicante(Nif, _, _) :: (findall(Nif, (adjudicante(Nif, _, _)), S), 
                                    length(S, N), 
                                    N == 1).

% Invariante que nao permite existir mais do que uma adjudicataria com o mesmo nif
+adjudicataria(Nif, _, _) :: (findall(Nif, (adjudicataria(Nif, _, _)), S), 
                                        length(S, N), 
                                        N == 1).

% Invariante que nao permite existir mais do que um contrato com o mesmo id
+contrato(Id, _, _, _, _, _, _, _, _, _, _, _) :: (findall(Id, (contrato(Id, _, _, _, _, _, _, _, _, _, _, _)), S), 
                                                                        length(S, N), 
                                                                        N == 1).

% Invariante que nao permite criar um contrato com um adjudicante ou adjudicataria nao existente
+contrato(_, Adjudicante, Adjudicataria, _, _, _, _, _, _, _, _, _) :: (findall(Adjudicante, (adjudicante(Adjudicante, _, _)), Se),
                                                                        length(Se, Ne),
                                                                        Ne == 1,
                                                                        findall(Adjudicataria, (adjudicataria(Adjudicataria, _, _)), Sa),
                                                                        length(Sa, Na),
                                                                        Na == 1).
                                                                    
% Invariante que garante a regra dos 3 anos
+contrato(Id, Ad, Ada, TC, TP, D, C, P, L, Dia, Mes, Ano) :: (findall(Custo, (contrato(_, Ad, Ada, _, _, _, Custo, _, _, _, _, A),
                                                                        A >= Ano - 3,
                                                                        A =< Ano,
                                                                        not(contrato(Id, Ad, Ada, TC, TP, D, C, P, L, Dia, Mes, Ano))),
                                                                        S),
                                                                sum(S, R),
                                                                R =< 75000).

% Invariante que garante que todos os contratos cumprem as normas impostas
+contrato(_, _, _, _, _, _, _, _, _, _, _, _) :: (findall(Id, (not(checkContract(contrato(Id, _, _, _, _, _, _, _, _, _, _, _)))), S),
                                            length(S, N),
                                            N == 0).

% Invariante que nao permite remover contratos
-contrato(_, _, _, _, _, _, _, _, _, _, _, _) :: fail.

% Invariante que nao permite remover adjudicantes com contratos associados
-adjudicante(Nif, _, _) :: (findall(Id, contrato(Id, Nif, _, _, _, _, _, _, _, _, _, _), S), length(S, 0)).

% Invariante que nao permite remover adjudicatarias com contratos associados
-adjudicataria(Nif, _, _) :: (findall(Id, contrato(Id, _, Nif, _, _, _, _, _, _, _, _, _), S), length(S, 0)).

% Predicado que verifica as condicoes para um contrato de ajuste direto
checkContract(contrato(_, _, "Aquisicao de servicos", ad, C, P, _, _, _, _)) :- 
    C =< 5000,
    P =< 365.
checkContract(contrato(_, _, "Aquisicao de bens moveis", ad, C, P, _, _, _, _)) :- 
    C =< 5000,
    P =< 365.
checkContract(contrato(_, _, "Aquisicao de bens moveis", ad, C, P, _, _, _, _)) :- 
    C =< 5000,
    P =< 365.
checkContract(contrato(_, _, _, ad, _, _, _, _, _, _)) :- 
    fail.
% Predicado que verifica se e um tipo de procedimento valido
checkContract(contrato(_, _, _, TP, _, _, _, _, _, _)) :- 
    tipoprocedimento(TP).
% Predicado que verifica se e uma data valida
checkContract(contrato(_, _, _, _, _, _, _, D, M, A)) :- 
    datavalida(D, M, A).
