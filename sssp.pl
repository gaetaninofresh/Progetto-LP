:- dynamic graph/1.
:- dynamic vertex/2.

%inserisco un nuovo grafo nella base-dati Prolog

new_graph(G) :-
    graph(G), !.

new_graph(G) :-
    assert(graph(G)), !.

%Rimuove tutto il grafo (vertici e archi inclusi) dalla base-dati Prolog

delete_graph(G) :-
    graph(G), !.

delete_graph(G) :-
    retract(graph(G)), !.

%Aggiunge il vertice V nella base-dati Prolog.
new_vertex(G, V) :-
    graph(G),
    atom(V),
    not(vertex(G, V)),
    assert(vertex(G, V)).

%Questo predicato è vero quanto Vs è una lista contenente tutti i vertici di G^4

vertices(G, Vs) :-
    setof(V, vertex(G,V), Vs), !.

vertices(G, Vs) :-
    is_list(Vs),
    setof(V, vertex(G,V), L),
    permutation(L, Vs), !.

% Questo predicato stampa alla console dell'interprete Prolog una lista
% dei vertici del grafo G

% list_vertices(G) - stampa tutti i vertici del grafo G
    %TODO: rimuovere stampa ":- dynamic vertex/2"

list_vertices(G) :-
    graph(G),
    listing(vertex(G, V)).

%new_edge(G, U, V, Weight) - Aggiunge un arco del grafo G alla base dati Prolog.
new_edge(G, U, V, Weight) :-
    assert(edge(G, U, V, Weight)).

new_edge(G, U, V) :-
    new_edge(G, U, V, 1).

%edges(G, Es) - Questo predicato è vero quando Es è una lista di tutti gli archi presenti in G.

edges(G, Es) :-
    setof(V, vertex(G,V), Es), !.

edges(G, Es) :-
    is_list(Es),
    setof(V, vertex(G,V), L),
    permutation(L, Es), !.

%neighbors(G, V, Ns) - Questo predicato è vero quando V è un vertice di G e Ns è una lista contenente gli archi, edge(G, V, N, W), che portano ai vertici N immediatamente raggiungibili da V 

%TODO: the rock controlla la funzione

neighbors(G, V, Ns) :-
    findall(N, edge(G, V, N, _), Ns).

%passo _ perchè il peso dei nodi non mi interssa

%list_edges(G) - Questo predicato stampa alla console dell’interprete Prolog una lista degli archi del grafo G

list_edges(G) :-
    graph(G),
    listing(edge(G, U, V, Weight)).
