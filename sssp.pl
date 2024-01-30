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
    listing(vertex(G, _V)).

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
    listing(edge(G, _U, _V, _Weight)).

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
% nello heap H con chiave K; nel caso in cui la chiave sia già presente 
% sostituisce il vecchio valore 


insert(H, K, V) :-
    retract(heap_entry(H, P, K, _Oldv)),
    assert(heap_entry(H, P, K, V)), !.

insert(H, K, V) :-
    % incrementa la dimensione dell'heap 
    heap_size(H, S),
    retract(heap(H, S)),
    NewS is S + 1,
    assert(heap(H, NewS)),
    assert(heap_entry(H, NewS, K, V)),
    heapify(H, NewS).

list_heap(H) :-
    listing(heap_entry(H, _I, _K, _V)).

% PREDICATI AGGIUNTIVI

% get_parent_index(I, Pi) - unifica Pi col valore della posizione del nodo 
% padre del nodo in posizione I

get_parent_index(I, Pi) :-
    I =< 1,
    Pi = 1, !. 

get_parent_index(I, Pi) :-
    I > 1,
    Pi is I / 2.


% implementazione delll'algoritmo di heapify
heapify(H, 0) :- fail.
heapify(H, 1) :- true.

heapify(H, I) :-
    get_parent_index(I, Ip),
    heap_entry(H, I, K, _V),
    heap_entry(H, Ip, Pk, _Vp),
    K > Pk.

heapify(H, I) :-
    get_parent_index(I, Ip),
    heap_entry(H, I, K, _V),
    heap_entry(H, Ip, Pk, _Vp),
    K < Pk,
    swap(H, I, Ip),
    heapify(H, Ip).

% scambia i nodi nell'heap H con indice I e IP 
swap(H, I, Ip) :-
    heap_entry(H, I, K, V),
    heap_entry(H, Ip, Kp, Vp),

    retract(heap_entry(H, I, K, V)),
    retract(heap_entry(H, Ip, Kp, Vp)),
    
    assert(heap_entry(H, Ip, Kp, Vp)),
    assert(heap_entry(H, I, K, V)).


% TEST
:- initialization(
    (
    new_heap(a),
    insert(a, 2, x),
    insert(a, 1, y),
    insert(a, 3, z),
    insert(a, 5, m),
    insert(a, 4, n),
    insert(a, 4, p)
    )
).