:- dynamic node/4.
:- dynamic paragem/11.

:- include('stops.pl').
:- include('edges.pl').

%--------------------------------- depth first
dfs(Nodo, Destino, [Nodo|Caminho]):-
    dfsr(Nodo, Destino, [Nodo], Caminho).

dfsr(Nodo, Destino, _, [Destino]):-
    adjacente(Nodo, Destino).

dfsr(Nodo, Destino, Visited, [ProxNodo|Caminho]):-
    adjacente(Nodo, ProxNodo),
    \+ member(ProxNodo, Visited),
    dfsr(ProxNodo, Destino, [Nodo|Visited], Caminho).

%--------------------------------- Greedy Search
greedy(Nodo, Destino, Caminho/Custo):-
    estima(Nodo, Destino, E),
    agreedy([[Nodo]/0/E], InvCaminho/Custo/_, Destino),
    inverso(InvCaminho,Caminho).

agreedy(Caminhos, Caminho, Destino):-
    get_best_g(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    Nodo == Destino.

agreedy(Caminhos, SolucaoCaminho, Destino):-
    get_best_g(Caminhos, MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expand_greedy(MelhorCaminho,ExpCaminhos, Destino),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agreedy(NovoCaminhos,SolucaoCaminho, Destino).

get_best_g([Caminho],Caminho):- !.

get_best_g([Caminho1/Custo1/Est1,_/_/Est2|Caminhos], MelhorCaminho):-
    Est1 =< Est2,
    !,
    get_best_g([Caminho1/Custo1/Est1|Caminhos],MelhorCaminho).

get_best_g([_|Caminhos], MelhorCaminho):-
    get_best_g(Caminhos, MelhorCaminho).

expand_greedy(Caminho, ExpCaminhos, Destino):-
    findall(NovoCaminho, adjacente(Caminho, NovoCaminho, Destino), ExpCaminhos).

estima(P1,P2,R):-
        paragem(P1,X1,Y1,_,_,_,_,_,_,_,_),
        paragem(P2,X2,Y2,_,_,_,_,_,_,_,_),
        R is sqrt(((X1-X2)*(X1-X2))+((Y1-Y2)*(Y1-Y2))).

%--------------------------------- depth first operators
dfs_op(Nodo, Destino, Operadoras, [Nodo|Caminho]):-
    dfsr_op(Nodo, Destino, Operadoras, [Nodo], Caminho).

dfsr_op(Nodo, Destino, Operadoras, _, [Destino]):-
    adjacente(Nodo, Destino),
    paragem(Nodo,_,_,_,_,_,O,_,_,_,_),
    member(O,Operadoras).

dfsr_op(Nodo, Destino, Operadoras, Visited, [ProxNodo|Caminho]):-
    adjacente(Nodo, ProxNodo),
    \+ member(ProxNodo, Visited),
    paragem(Nodo,_,_,_,_,_,O,_,_,_,_),
    member(O,Operadoras),
    dfsr_op(ProxNodo, Destino, Operadoras, [Nodo|Visited], Caminho).


%--------------------------------- depth first no operators
dfs_noop(Nodo, Destino, Operadoras, [Nodo|Caminho]):-
    dfsr_op(Nodo, Destino, Operadoras, [Nodo], Caminho).

dfsr_noop(Nodo, Destino, _, _, [Destino]):-
    adjacente(Nodo, Destino).

dfsr_noop(Nodo, Destino, Operadoras, Visited, [ProxNodo|Caminho]):-
    adjacente(Nodo, ProxNodo),
    \+ member(ProxNodo, Visited),
    paragem(Nodo,_,_,_,_,_,O,_,_,_,_),
    \+ member(O,Operadoras),
    dfsr_noop(ProxNodo, Destino, Operadoras, [Nodo|Visited], Caminho).

%--------------------------------- depth first sorted carreiras
sorted_nstop(Nodo, Destino, R):-
    dfs(Nodo, Destino, P),
    calc_len(P, PL),
    quick_sort(PL, R).

calc_len([], []).
calc_len([H|T], [(H,LC)|R]):-
    paragem(H,_,_,_,_,_,_,C,_,_,_),
    length(C, LC),
    calc_len(T, R).

quick_sort([],[]).
quick_sort([H|T],Sorted):-
	pivoting(H,T,L1,L2),
        quick_sort(L1,Sorted1),
        quick_sort(L2,Sorted2),
	append(Sorted1,[H|Sorted2], Sorted).
   
pivoting(_,[], [], []).
pivoting((H,C1),[(X,C2)|T], L, [(X,C2)|G]):-
    C2=<C1,
    pivoting((H,C1),T,L,G).
pivoting((H, C1),[(X,C2)|T], [(X, C2)|L], G):-
    C2>C1,
    pivoting((H,C1), T, L, G).

%--------------------------------- best len TODO
best_len(Nodo, Destino, S, N_stops) :-
    findall((SS, CC), dfs_len(Nodo, Destino, SS, CC), L),
    minimo(L,(S,N_stops)).

dfs_len(Nodo, Destino, C, N):-
    dfs(Nodo, Destino, C),
    length(C, N).

%--------------------------------- best cost TODO
best_cost(Nodo, Destino, S, Custo):-
    findall((SS, CC), dfs_cost(Nodo, Destino, SS, CC), L),
    minimo(L,(S,Custo)).

dfs_cost(Nodo, Destino, Path, Custo):-
    dfs(Nodo, Destino, Path),
    calc_cost(Path, Custo).

calc_cost([H,NH], C):-
    edge(H, NH, _, C).
calc_cost([H,NH|T], C):-
    edge(H, NH, _, C1),
    calc_cost([NH|T], C2),
    C is C1+ C2.

%--------------------------------- depth first publicidade
dfs_pub(Nodo, Destino, [Nodo|Caminho]):-
    dfsr_abrigado(Nodo, Destino, [Nodo], Caminho).

dfsr_pub(Nodo, Destino, _, [Destino]):-
    adjacente(Nodo, Destino).

dfsr_pub(Nodo, Destino, Visited, [ProxNodo|Caminho]):-
    adjacente(Nodo, ProxNodo),
    \+ member(ProxNodo, Visited),
    paragem(Nodo,_,_,_,_,_,_,P,_,_,_),
    P == 'Yes',
    dfsr_pub(ProxNodo, Destino, [Nodo|Visited], Caminho).

%--------------------------------- depth first abrigado
dfs_abrigado(Nodo, Destino, [Nodo|Caminho]):-
    dfsr_abrigado(Nodo, Destino, [Nodo], Caminho).

dfsr_abrigado(Nodo, Destino, _, [Destino]):-
    adjacente(Nodo, Destino).

dfsr_abrigado(Nodo, Destino, Visited, [ProxNodo|Caminho]):-
    adjacente(Nodo, ProxNodo),
    \+ member(ProxNodo, Visited),
    paragem(Nodo,_,_,_,_,_,A,_,_,_,_),
    A \== 'Sem Abrigo',
    dfsr_abrigado(ProxNodo, Destino, [Nodo|Visited], Caminho).

%--------------------------------- intermediate points
inter([P1, P2], Curr):-
    greedy(P1, P2, Curr/_).

inter([P1, P2 | Rest], Path):-
    greedy(P1, P2, Curr/_),
    inter([P2 | Rest], Tail),
    init(Curr, Initial),
    append(Initial, Tail, Path).

init([], []).
init([_], []).
init([H|T], [H|R]):- init(T, R).

%--------------------------------- auxiliars
adjacente(Nodo,ProxNodo):-
    edge(Nodo, ProxNodo, _, _).

adjacente([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/Est, Destino):-
    edge(Nodo, ProxNodo, _, PassoCusto),
    \+ member(ProxNodo, Caminho),
    NovoCusto is Custo + PassoCusto,
    estima(ProxNodo, Destino, Est).

inverso(Xs, Ys):- inverso(Xs, [], Ys).

inverso([], Xs, Xs).
inverso([X|Xs],Ys, Zs):- inverso(Xs, [X|Ys], Zs).

seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]):- seleciona(E, Xs, Ys).


minimo([H|T], R):- minimoa(T, H, R).

minimoa([], Min, Min).
minimoa([(_, C) | T], (NM, CM), Min):-
    C > CM,
    minimoa(T, (NM, CM) , Min).

minimoa([(H, C) | T], (_, CM), Min):-
    C =< CM,
    minimoa(T, (H, C), Min).
