% per evitare warning
:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic edge/4.
:- dynamic heap/2.
:- dynamic heap_entry/4.

% GRAPH
% new_graph(G) - aggiunge il grafo G alla base di conoscenza
new_graph(G) :- 
    graph(G), !.

new_graph(G) :-
    assert(graph(G)), !.

% delete_graph(G) - rimuove il grafo G dalla base di conoscenza
delete_graph(G) :-
    graph(G),
    retractall(edge(G, _, _, _)),
    retractall(vertex(G, _)),
    retract(graph(G)).
  
% new_vertex(G, V) - crea un nuovo vertice V nel grafo G   
new_vertex(G, V) :-
    graph(G),
    atom(V),
    not(vertex(G, V)),
    assert(vertex(G, V)).

% vertices(G, Vs) - ritorna ver/falso se Vs è la lista di tutti i vertici di G 
vertices(G, Vs) :-
    setof(V, vertex(G,V), Vs), !.

vertices(G, Vs) :-
    is_list(Vs),
    setof(V, vertex(G,V), L),
    permutation(L, Vs), !.

% list_vertices(G) - stampa tutti i vertici del grafo G
    %TODO: rimuovere stampa ":- dynamic vertex/2"
list_vertices(G) :-
    graph(G),
    listing(vertex(G, V)).

% new_edge(G, U, V, Weight) - Aggiunge un arco del grafo G 
%   alla base dati Prolog.
new_edge(G, U, V, Weight) :-
    assert(edge(G, U, V, Weight)).

new_edge(G, U, V) :-
    new_edge(G, U, V, 1).

% edges(G, Es) - Questo predicato è vero quando Es è una lista di tutti
%   gli archi presenti in G.
edges(G, Es) :-
    setof(V, vertex(G,V), Es), !.

edges(G, Es) :-
    is_list(Es),
    setof(V, vertex(G,V), L),
    permutation(L, Es), !.

% neighbors(G, V, Ns) - Questo predicato è vero quando V è un vertice di G e 
%   Ns è una lista contenente gli archi, edge(G, V, N, W), 
%   che portano ai vertici N immediatamente raggiungibili da V 

% TODO: the rock controlla la funzione
neighbors(G, V, Ns) :-
    findall(N, edge(G, V, N, _), Ns).
    % passo _ perchè il peso dei nodi non mi interssa

% list_edges(G) - Questo predicato stampa alla console dell’interprete Prolog
%   una lista degli archi del grafo G
list_edges(G) :-
    graph(G),
    listing(edge(G, U, V, Weight)).

% list_graph(G) - Stampa una lista dei vertici e degli archi del grafo G.
list_graph(G) :-
    graph(G),
    list_vertices(G),
    list_edges(G).

% MINHEAP

% new_heap(H) - crea un nuovo heap H e lo inserisce nella base di conoscenza
% TODO: capire che vuol dire "mantiene la dimensione corrente dello heap nel
% secondo argomento."
new_heap(H) :- heap(H, _S), !.
new_heap(H) :- assert(heap(H, 0)), !.

% delete_heap(H) - elimina l'heap H dalla base di conoscenza
delete_heap(H) :- 
    retractall(heap_entry(H, _, _, _)),
    retract(heap(H, _)).
    

% heap_size(H) - ritorna true se l'heap H ha dimensione S 
heap_size(H, S) :- 
    heap(H, S).

% empty(H) - ritorna true se l'heap H è vuoto
empty(H) :-
    heap(H, 0).

% not_empty(H) - ritorna true se l'heap H non è vuoto
not_empty(H) :-
    not(heap(H, 0)).

% head(H, K, V) - è vero quando l’elemento dello heap H con chiave minima K è V
head(H, K, V) :-
    heap_entry(H, 1, K, V).

% insert(H, K, V) -  è vero quando l’elemento V è inserito 
% nello heap H con chiave K

% caso heap vuoto
insert(H, K, V) :-
    empty(H),
    assert(heap_entry(H, 1, K, V)),
    retract(heap(H, 0)),
    assert(heap(H,1)).

% PREDICATI AGGIUNTIVI

% find_insert_pos(H, K, V) - trova la posizione P nell 'heap H 
%    in cui inserire V con chiave K 

find_insert_pos(H, K, V, I, R) :-
    heap_entry(H, In, Kn, _),
    K @< Kn,
    R = I.



