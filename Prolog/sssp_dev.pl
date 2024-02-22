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

% vertices(G, Vs) - ritorna ver/falso se Vs è la lista di tutti i vertici di G 
vertices(G, Vs) :-
    findall(V, vertex(G,V), Vs), !.

vertices(G, Vs) :-
    is_list(Vs),
    findall(V, vertex(G,V), L),
    permutation(L, Vs), !.

% list_vertices(G) - stampa tutti i vertici del grafo G
    %TODO: rimuovere stampa ":- dynamic vertex/2"
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

% edges(G, Es) - Questo predicato è vero quando Es è una lista di tutti
%   gli archi presenti in G.

edges(G, Es) :-
    findall(edge(G, U, V, W), edge(G, U, V, W), Es), !.

edges(G, Es) :-
    is_list(Es),
    findall(edge(G, U, V, W), edge(G, U, V, W), L),
    permutation(L, Es), !.

% neighbors(G, V, Ns) - Questo predicato è vero quando V è un vertice di G e 
%   Ns è una lista contenente gli archi, edge(G, V, N, W), 
%   che portano ai vertici N immediatamente raggiungibili da V, il grafo viene
%   considerato non orientato 

neighbors(G, V, Ns) :-
    % TODO: verificare bidirezionalità degli archi
    /*
    findall(N, edge(G, V, N, _), L1),
    findall(N, edge(G, N, V, _), L2),
    union(L1, L2, Ns).
    */
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

% insert(H, K, V) - è vero quando l’elemento V è inserito 
% nello heap H con chiave K; non fa nulla se proviamo a reinserire la 

% stessa coppia (K, V)

insert(H, K, V) :-
    heap_entry(H, _P, K, V), !.

/*
insert(H, K, V) :-
    retract(heap_entry(H, P, K, _Oldv)),
    assert(heap_entry(H, P, K, V)), !.
*/

insert(H, K, V) :-
    % incrementa la dimensione dell'heap 
    heap_size(H, S),
    retract(heap(H, S)),
    NewS is S + 1,
    assert(heap(H, NewS)),
    
    assert(heap_entry(H, NewS, K, V)),
    heapify_up(H, NewS), !.

% extract(H, K, V) - è vero quando la coppia K, V con K minima, è rimossa 
% dallo heap H
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

% modify_key(H, NewK, OldK, V) - è vero quando la chiave OldKey 
% (associata al valore V) è sostituita
% da NewKey

modify_key(H, NewK, NewK, _V) :- heap(H, _).

modify_key(H, NewK, OldK, V) :-
    retract(heap_entry(H, P, OldK, V)),
    assert(heap_entry(H, P, NewK, V)),
    heap(H, S),
    heapify_up(H, S),
    heapify(H, 1), !.

list_heap(H) :-
    listing(heap_entry(H, _I, _K, _V)).

% DIJKSTRA
% change_distance(G, V, NewDist) - ha sempre successo con due effetti 
% collaterali: prima tutte le istanze di distance(G, V, _) vengono ritratte
% e distance(G, V,NewDist) è asserita. 
change_distance(G, V, NewDist) :-
    graph(G),
    nonvar(V),
    retractall(distance(G, V, _)),
    assert(distance(G, V, NewDist)).

% change_previous(G, V, U) - ha successo con due effetti collaterali: 
% prima tutte le istanze di previous(G, V, _) sono ritirate e 
% previous(G, V, U) è asserita.
change_previous(G, V, U) :-
    graph(G),
    nonvar(V),
    nonvar(U),
    retractall(previous(G, V, _)),
    assert(previous(G, V, U)).

% set_visited(G, V) - ha successo con un effetto collaterale: asserisce V come 
% visitato
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

%versione nome
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


shortest_path(G, Source, Source, []) :- !.

shortest_path(G, Source, V, Path) :-
    graph(G),
    nonvar(V),
    nonvar(Source),

    previous(G, V, Prev),
    edge(G, Prev, V, W),
    shortest_path(G, Source, Prev, PrevPath),
    append(PrevPath, [edge(G, Prev, V, W)], Path), !.

% PREDICATI AGGIUNTIVI

% get_parent_index(I, Pi) - unifica Pi col valore della posizione del nodo 
% padre del nodo in posizione I
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


% TEST
:- initialization (
    (       
        new_graph(g),
        new_vertex(g, a),
        new_vertex(g, b),
        new_vertex(g, c),
        new_vertex(g, d),
        new_vertex(g, e),
        new_vertex(g, f),
        new_vertex(g, h),
        
        new_edge(g, vertex(g, a), vertex(g, b), 3),
        
        new_edge(g, vertex(g, a), vertex(g, e), 11),
        
        new_edge(g, vertex(g, a), vertex(g, d), 4),
        
        new_edge(g, vertex(g, b), vertex(g, c), 5),
        
        new_edge(g, vertex(g, d), vertex(g, c), 7),
        
        new_edge(g, vertex(g, e), vertex(g, d), 6),
        
        new_edge(g, vertex(g, a), vertex(g, f), 3),
        
        new_edge(g, vertex(g, c), vertex(g, h), 1),
        
        list_edges(g),


        new_graph(g1),
        new_vertex(g1, a),
        new_vertex(g1, b),
        new_vertex(g1, c),
        new_vertex(g1, d),
        new_vertex(g1, e),
        new_vertex(g1, f),
        new_vertex(g1, h),
        
        new_edge(g1, a, b, 3),
        new_edge(g1, a, e, 11),
        new_edge(g1, a, d, 4),
        new_edge(g1, b, c, 5),
        new_edge(g1, d, c, 7),
        new_edge(g1, e, d, 6),
        new_edge(g1, a, f, 3),
        new_edge(g1, c, h, 1),
        
        list_edges(g1),

        
        new_graph(big),

        new_vertex(big, a),
        new_vertex(big, b),
        new_vertex(big, c),
        new_vertex(big, d),
        new_vertex(big, e),
        new_vertex(big, f),
        new_vertex(big, h),
        new_vertex(big, i),
        new_vertex(big, j),
        new_vertex(big, k),
        new_vertex(big, l),
        new_vertex(big, m),
        new_vertex(big, n),
        new_vertex(big, o),
        new_vertex(big, p),
        new_vertex(big, q),
        new_vertex(big, r),
        new_vertex(big, s),
        new_vertex(big, t),
        new_vertex(big, u),
        new_vertex(big, v),
        new_vertex(big, w),
        new_vertex(big, x),
        new_vertex(big, y),
        new_vertex(big, z),
        new_vertex(big, aa),
        new_vertex(big, bb),
        new_vertex(big, cc),
        new_vertex(big, dd),
        new_vertex(big, ee),
        new_vertex(big, ff),
        new_vertex(big, gg),
        new_vertex(big, hh),
        new_vertex(big, ii),
        new_vertex(big, jj),
        new_vertex(big, kk),
        new_vertex(big, ll),
        new_vertex(big, mm),
        new_vertex(big, nn),
        new_vertex(big, oo),
        new_vertex(big, pp),
        new_vertex(big, qq),
        new_vertex(big, rr),
        new_vertex(big, ss),
        new_vertex(big, tt),
        new_vertex(big, uu),
        new_vertex(big, vv),
        new_vertex(big, ww),
        new_vertex(big, xx),
        new_vertex(big, yy),
        new_vertex(big, zz),
        new_edge(big, vertex(big, a), vertex(big, b), 7),
        new_edge(big, vertex(big, a), vertex(big, e), 29),
        new_edge(big, vertex(big, a), vertex(big, d), 13),
        new_edge(big, vertex(big, b), vertex(big, c), 45),
        new_edge(big, vertex(big, d), vertex(big, c), 62),
        new_edge(big, vertex(big, e), vertex(big, d), 18),
        new_edge(big, vertex(big, a), vertex(big, f), 9),
        new_edge(big, vertex(big, c), vertex(big, h), 37),
        new_edge(big, vertex(big, i), vertex(big, j), 52),
        new_edge(big, vertex(big, j), vertex(big, k), 31),
        new_edge(big, vertex(big, k), vertex(big, l), 49),
        new_edge(big, vertex(big, l), vertex(big, m), 16),
        new_edge(big, vertex(big, m), vertex(big, n), 41),
        new_edge(big, vertex(big, n), vertex(big, o), 56),
        new_edge(big, vertex(big, o), vertex(big, p), 23),
        new_edge(big, vertex(big, p), vertex(big, q), 37),
        new_edge(big, vertex(big, q), vertex(big, r), 28),
        new_edge(big, vertex(big, r), vertex(big, s), 19),
        new_edge(big, vertex(big, s), vertex(big, t), 14),
        new_edge(big, vertex(big, t), vertex(big, u), 11),
        new_edge(big, vertex(big, u), vertex(big, v), 8),
        new_edge(big, vertex(big, v), vertex(big, w), 5),
        new_edge(big, vertex(big, w), vertex(big, x), 32),
        new_edge(big, vertex(big, x), vertex(big, y), 27),
        new_edge(big, vertex(big, y), vertex(big, z), 20),
        new_edge(big, vertex(big, aa), vertex(big, bb), 46),
        new_edge(big, vertex(big, cc), vertex(big, dd), 54),
        new_edge(big, vertex(big, ee), vertex(big, ff), 63),
        new_edge(big, vertex(big, gg), vertex(big, hh), 72),
        new_edge(big, vertex(big, ii), vertex(big, jj), 81),
        new_edge(big, vertex(big, kk), vertex(big, ll), 90),
        new_edge(big, vertex(big, mm), vertex(big, nn), 2),
        new_edge(big, vertex(big, oo), vertex(big, pp), 15),
        new_edge(big, vertex(big, qq), vertex(big, rr), 35),
        new_edge(big, vertex(big, ss), vertex(big, tt), 47),
        new_edge(big, vertex(big, uu), vertex(big, vv), 60),
        new_edge(big, vertex(big, ww), vertex(big, xx), 73),
        new_edge(big, vertex(big, yy), vertex(big, zz), 88), 

        list_edges(big)
    )
).