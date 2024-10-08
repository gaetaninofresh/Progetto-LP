
% Gaetano La Rocca, matricola 895887
% Filippo Gentili, matricola 899906
% Luca Giandomenico, matricola 900162


% per evitare warning
:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic edge/4.
:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic visited/2.
:- dynamic distance/3.
:- dynamic previous/3.

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

% vertices(G, Vs) - ritorna vero/falso se Vs
% e' la lista di tutti i vertici di G 
vertices(G, Vs) :-
    findall(V, vertex(G,V), Vs), !.

vertices(G, Vs) :-
    is_list(Vs),
    findall(V, vertex(G,V), L),
    permutation(L, Vs), !.

% list_vertices(G) - stampa tutti i vertici del grafo G
list_vertices(G) :-
    graph(G),
    listing(vertex(G, _V)).

% new_edge(G, U, V, Weight) - Aggiunge un arco del grafo G 
%   alla base dati Prolog.
new_edge(G, U, V, Weight) :-
    nonvar(V),
    nonvar(U),
    Weight >= 1,
    assert(edge(G, U, V, Weight)).

new_edge(G, U, V) :-
    nonvar(V),
    nonvar(U),
    new_edge(G, U, V, 1).

% edges(G, Es) - Questo predicato e' vero quando
% Es e' una lista di tutti gli archi presenti in G.

edges(G, Es) :-
    findall(edge(G, U, V, W), edge(G, U, V, W), Es), !.

edges(G, Es) :-
    is_list(Es),
    findall(edge(G, U, V, W), edge(G, U, V, W), L),
    permutation(L, Es), !.

% neighbors(G, V, Ns) - Questo predicato e' vero quando V
% e' un vertice di G e 
%  Ns e' una lista contenente gli archi, edge(G, V, N, W), 
%  che portano ai vertici N immediatamente raggiungibili da V
neighbors(G, V, Ns) :-
    findall(N, edge(G, V, N, _), Ns).


% passo _ perche' il peso dei nodi non mi interssa

% list_edges(G) - Questo predicato stampa alla
% console dell'interprete Prolog
%  una lista degli archi del grafo G
list_edges(G) :-
    graph(G),
    listing(edge(G, _U, _V, _Weight)).

% list_graph(G) - Stampa una lista dei vertici e
% degli archi del grafo G.
list_graph(G) :-
    graph(G),
    list_vertices(G),
    list_edges(G).



% MINHEAP

% new_heap(H) - crea un nuovo heap H e lo inserisce
% nella base di conoscenza
new_heap(H) :- heap(H, _S), !.
new_heap(H) :- assert(heap(H, 0)), !.

% delete_heap(H) - elimina l'heap H dalla base di conoscenza
delete_heap(H) :- 
    retractall(heap_entry(H, _, _, _)),
    retract(heap(H, _)).


% heap_size(H) - ritorna true se l'heap H ha dimensione S 
heap_size(H, S) :- 
    heap(H, S).

% empty(H) - ritorna true se l'heap H e' vuoto
empty(H) :-
    heap(H, 0).

% not_empty(H) - ritorna true se l'heap H non e' vuoto
not_empty(H) :-
    not(heap(H, 0)).

% head(H, K, V) - e' vero quando l'elemento dello
% heap H con chiave minima K e' V
head(H, K, V) :-
    heap_entry(H, 1, K, V).

% insert(H, K, V) - e' vero quando l'elemento V e' inserito 
% nello heap H con chiave K; non fa nulla se proviamo a  
% reinserire la stessa coppia (K, V)

insert(H, K, V) :-
    heap_entry(H, _P, K, V), !.

insert(H, K, V) :-
    % incrementa la dimensione dell'heap 
    heap_size(H, S),
    retract(heap(H, S)),
    NewS is S + 1,
    assert(heap(H, NewS)),
    
    assert(heap_entry(H, NewS, K, V)),
    heapify_up(H, NewS), !.

% extract(H, K, V) - e' vero quando la coppia K,
% V con K minima, e' rimossa dallo heap H
extract(H, _, _) :-
    empty(H).

extract(H, K, V) :-
    heap_size(H, 1),
    retract(heap_entry(H, 1, K, V)),
    retract(heap(H, 1)),
    assert(heap(H, 0)).

extract(H, K, V) :-
    heap_size(H, S),
    swap(H, 1, S),

    retract(heap_entry(H, S, K, V)),

    % aggiorna la dimensione dello heap
    retract(heap(H, S)),
    NewS is S - 1,
    assert(heap(H, NewS)),
    
    heapify_up(H, NewS),
    heapify(H, 1), !.

% modify_key(H, NewK, OldK, V) - e' vero quando la chiave OldKey 
% (associata al valore V) e' sostituita
% da NewKey

modify_key(H, NewK, NewK, _V) :- heap(H, _).

modify_key(H, NewK, OldK, V) :-
    retract(heap_entry(H, P, OldK, V)),
    assert(heap_entry(H, P, NewK, V)),
    heapify_up(H, P),
    heapify(H, P), !.

list_heap(H) :-
    listing(heap_entry(H, _I, _K, _V)).

% DIJKSTRA
% change_distance(G, V, NewDist) - ha
% sempre successo con due effetti 
% collaterali: prima tutte le istanze di distance(G, V, _)
% vengono ritratte e distance(G, V,NewDist) e' asserita. 
change_distance(G, V, NewDist) :-
    graph(G),
    nonvar(V),
    retractall(distance(G, V, _)),
    assert(distance(G, V, NewDist)).

% change_previous(G, V, U) - ha successo con due
% effetti collaterali: 
% prima tutte le istanze di previous(G, V, _) sono ritirate e 
% previous(G, V, U) e' asserita.
change_previous(G, V, U) :-
    graph(G),
    nonvar(V),
    nonvar(U),
    retractall(previous(G, V, _)),
    assert(previous(G, V, U)).

% set_visited(G, V) - ha successo con un effetto collaterale:
% asserisce V come visitato
set_visited(G, V) :-
    graph(G),
    nonvar(V),
    retractall(visited(G, V)),
    assert(visited(G, V)).




%versione vertex(g, v)
init_sssp(G, Source) :-
    
    \+ atom(Source),

    retractall(visited(G, V)),
    retractall(previous(G, V, _)),
    
    forall(vertex(G, V),
           change_distance(G, vertex(G, V), inf)
	  ),
    change_distance(G, Source, 0),
    set_visited(G, Source).

%versione solo nome
init_sssp(G, Source) :-
    
    vertex(G, Source),

    retractall(visited(G, V)),
    retractall(previous(G, V, _)),
    
    forall(vertex(G, V),
           change_distance(G, V, inf)
	  ),
    change_distance(G, Source, 0),
    set_visited(G, Source).

dijkstra(G, Source, Heap) :-
    set_visited(G, Source),
    extract(Heap, _, _),
    findall(
        V,
        visited(G, V),
        Visited
    ),
    
    findall(
        V,
        (
            member(Exp, Visited),
            neighbors(G, Exp, N),
            member(V, N),
            \+ visited(G, V)
        ),
        L
    ),
    %removes unwanted duplicates
    sort(L, Neighbors),
    
    forall(
        (
            member(Node, Neighbors),
            member(Exp, Visited),
            edge(G, Exp, Node, _)
        ),
        (
            dijkstra_check_cost(G, Node, Exp, NewDist),
            dijkstra_heap_insert(H, NewDist, Node)
        )
    ),
    head(H, _, Head),
    dijkstra(G, Head, H), !.

dijkstra(_, _, Heap) :-
    empty(Heap), !.

dijkstra_sssp(G, Source) :-
    graph(G),
    nonvar(Source),
    init_sssp(G, Source),

    new_heap(heap_dijkstra_sssp),
    
    insert(heap_dijkstra_sssp, 0, Source),
    dijkstra(G, Source, heap_dijkstra_sssp), !.


shortest_path(G, Source, Source, []) :- graph(G), !.

shortest_path(G, Source, V, Path) :-
    graph(G),
    nonvar(V),
    nonvar(Source),

    previous(G, V, Prev),
    edge(G, Prev, V, W),
    shortest_path(G, Source, Prev, PrevPath),
    append(PrevPath, [edge(G, Prev, V, W)], Path), !.

% PREDICATI AGGIUNTIVI

% get_parent_index(I, Pi) - unifica Pi col valore della
% posizione del nodo padre del nodo in posizione I
get_parent_index(I, Pi) :-
    I =< 1,
    Pi = 1, !.

get_parent_index(I, Pi) :-
    I > 1,
    Pi is I div 2.


% implementazione delll'algoritmo di heapify e heapify_up
heapify(H, Node) :-
    heap_entry(H, Node, _, _),
    heap_size(H, Size),
    left_child(Node, Left),
    Left > Size, !.

heapify(H, Node) :-
    heap_entry(H, Node, NodeK, _),
    heap_size(H, Size),
    left_child(Node, Left),
    Left = Size,
    heap_entry(H, Left, LeftK, _),
    LeftK >= NodeK, !.

heapify(H, Node) :-
    heap_entry(H, Node, NodeK, _),
    heap_size(H, Size),
    left_child(Node, Left),
    Left = Size,
    heap_entry(H, Left, LeftK, _),
    LeftK < NodeK,
    swap(H, Node, Left),
    heapify(H, Left), !.

heapify(H, Node) :-
    heap_entry(H, Node, NodeK, _),
    left_child(Node, Left),
    heap_entry(H, Left, LeftK, _),
    LeftK >= NodeK,
    right_child(Node, Right),
    heap_entry(H, Right, RightK, _),
    RightK >= NodeK, !.

heapify(H, Node) :-
    heap_entry(H, Node, NodeK, _),
    left_child(Node, Left),
    heap_entry(H, Left, LeftK, _),
    LeftK >= NodeK,
    right_child(Node, Right),
    heap_entry(H, Right, RightK, _),
    RightK < NodeK,
    swap(H, Node, Right),
    heapify(H, Right), !.

heapify(H, Node) :-
    heap_entry(H, Node, NodeK, _),
    left_child(Node, Left),
    heap_entry(H, Left, LeftK, _),
    LeftK < NodeK,
    right_child(Node, Right),
    heap_entry(H, Right, RightK, _),
    RightK >= NodeK,
    swap(H, Node, Left),
    heapify(H, Left), !.

heapify(H, Node) :-
    heap_entry(H, Node, NodeK, _),
    left_child(Node, Left),
    heap_entry(H, Left, LeftK, _),
    LeftK < NodeK,
    right_child(Node, Right),
    heap_entry(H, Right, RightK, _),
    RightK < NodeK,
    LeftK =< RightK,
    swap(H, Node, Left),
    heapify(H, Left), !.

heapify(H, Node) :-
    heap_entry(H, Node, NodeK, _),
    left_child(Node, Left),
    heap_entry(H, Left, LeftK, _),
    LeftK < NodeK,
    right_child(Node, Right),
    heap_entry(H, Right, RightK, _),
    RightK < NodeK,
    LeftK > RightK,
    swap(H, Node, Right),
    heapify(H, Right), !.


heapify_up(_, 1) :- !.
heapify_up(H, Node) :-
    heap_entry(H, Node, NodeK, _),
    get_parent_index(Node, Parent),
    heap_entry(H, Parent, ParentK, _),
    ParentK =< NodeK, !.

heapify_up(H, Node) :-
    heap_entry(H, Node, _, _),
    get_parent_index(Node, Parent),
    swap(H, Parent, Node),
    heapify_up(H, Parent), !.


% scambia i nodi nell'heap H con indice I e IP 
swap(H, I, I) :- heap(H, _).

swap(H, I, Ip) :-
    heap_entry(H, I, K, V),
    heap_entry(H, Ip, Kp, Vp),

    retract(heap_entry(H, I, K, V)),
    retract(heap_entry(H, Ip, Kp, Vp)),
    
    asserta(heap_entry(H, Ip, K, V)),
    asserta(heap_entry(H, I, Kp, Vp)).

dijkstra_heap_insert(H, K, V) :-
    heap_entry(H, _, OldK, V),
    modify_key(H, K, OldK, V), !.

dijkstra_heap_insert(H, K, V) :-
    insert(H, K, V), !.

dijkstra_check_cost(G, V, Prev, R) :-
    distance(G, Prev, PrevCost),
    distance(G, V, OldDist),
    edge(G, Prev, V, Cost),
    NewDist is Cost + PrevCost,
    NewDist < OldDist,
    R = NewDist,
    change_distance(G, V, NewDist),
    change_previous(G, V, Prev), !.

dijkstra_check_cost(G, V, Prev, R) :-
    distance(G, Prev, PrevCost),
    distance(G, V, OldDist),
    edge(G, Prev, V, Cost),
    NewDist is Cost + PrevCost,
    NewDist >= OldDist,
    R = OldDist, !.

left_child(Pi, I) :-
    I is 2 * Pi.

right_child(Pi, I) :-
    I is 2 * Pi + 1.
