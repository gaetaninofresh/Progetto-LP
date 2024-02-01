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
    nonvar(V),
    nonvar(U),
    Weight > 1,
    assert(edge(G, U, V, Weight)).

new_edge(G, U, V) :-
    nonvar(V),
    nonvar(U),
    new_edge(G, U, V, 1).

% edges(G, Es) - Questo predicato è vero quando Es è una lista di tutti
%   gli archi presenti in G.
% TODO: verificare funzionamento
edges(G, Es) :-
    setof(_, edge(G,_U, _V, _W), Es), !.

edges(G, Es) :-
    is_list(Es),
    setof(_, edge(G,_U, _V, _W), L),
    permutation(L, Es), !.

% neighbors(G, V, Ns) - Questo predicato è vero quando V è un vertice di G e 
%   Ns è una lista contenente gli archi, edge(G, V, N, W), 
%   che portano ai vertici N immediatamente raggiungibili da V, il grafo viene
%   considerato non orientato 

neighbors(G, V, Ns) :-
    findall(N, edge(G, V, N, _), L1),
    findall(N, edge(G, N, V, _), L2),
    union(L1, L2, Ns).
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

% extract(H, K, V) - è vero quando la coppia K, V con K minima, è rimossa 
% dallo heap H
extract(H, K, V) :-
    heap_size(H, S),
    swap(H, 1, S),

    retract(heap_entry(H, S, K, V)),

    % aggiorna la dimensione dello heap
    retract(heap(H, S)),
    NewS is S - 1,
    assert(heap(H, NewS)),
    
    heapify(H, NewS).

% modify_key(H, NewK, OldK, V) - è vero quando la chiave OldKey 
% (associata al valore V) è sostituita
% da NewKey

modify_key(H, NewK, NewK, _V) :- heap(H, _).

modify_key(H, NewK, OldK, V) :-
    retract(heap_entry(H, P, OldK, V)),
    assert(heap_entry(H, P, NewK, V)),
    heap(H, S),
    heapify(H, S).

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



init_sssp(G, Source) :-
    
    retractall(visited(G, V)),
    retractall(previous(G, V, _)),
    
    forall(vertex(G, V),
        change_distance(G, vertex(G, V), inf)
    ),
    change_distance(G, Source, 0),
    set_visited(G, Source).


dijkstra(G, Source, Heap) :-
    empty(Heap), !.


dijkstra(G, Source, Heap) :-
    set_visited(G, Source),
    neighbors(G, Source, Ns),
    extract(H, _, V),
    forall(
        (
            member(N, Ns),
            \+ visited(G, N),
            distance(G, N, D),
            D == inf,
            insert(H, Dist, V)
        ),
        (
            edge(G, Source, N, D),
            distance(G, Source, Sdist),
            Dist is D + Sdist,
            change_distance(G, N, Dist),
            change_previous(G, N, Source)
        )
    ),
    list_heap(Heap),
    dijkstra(G, V, H).

dijkstra(G, Source, Heap) :-
    set_visited(G, Source),
    neighbors(G, Source, Ns),
    extract(H, _, V),
    forall(
        (
            member(N, Ns),
            not(visited(G, N)),
            insert(H, K, V)
        ),
        (
            distance(G, N, OldDist),
            distance(G, Source, SDist),
            edge(G, Source, N, Cost),
            NewDist is Cost + SDist,
            NewDist < OldDist,
            change_distance(G, N, NewDist),
            change_previous(G, N, Source)
        )
    ),
    list_heap(Heap),
    dijkstra(G, V, H).
    
dijkstra_sssp(G, Source) :-
    graph(G),
    nonvar(Source),
    init_sssp(G, Source),

    new_heap(h),
    distance(G, Source, Key),
    insert(h, Key, Source).
    %dijkstra(G, Source, h).


% PREDICATI AGGIUNTIVI

% get_parent_index(I, Pi) - unifica Pi col valore della posizione del nodo 
% padre del nodo in posizione I
get_parent_index(I, Pi) :-
    I =< 1,
    Pi = null, !. 

get_parent_index(I, Pi) :-
    I > 1,
    Pi is I div 2.


% implementazione delll'algoritmo di heapify
heapify(H, 0) :- heap(H, _), fail.
heapify(H, 1) :- heap(H, _), true.
heapify(H, null) :- true, !.

heapify(H, I) :-
    heap(H, _),
    get_parent_index(I, Ip),
    heap_entry(H, I, K, _V),
    heap_entry(H, Ip, Pk, _Vp),
    K > Pk, 
    heapify(H, Ip).

heapify(H, I) :-
    heap(H, _),
    get_parent_index(I, Ip),
    heap_entry(H, I, K, _V),
    heap_entry(H, Ip, Pk, _Vp),
    K < Pk,
    swap(H, I, Ip),
    heapify(H, Ip).

% scambia i nodi nell'heap H con indice I e IP 
swap(H, I, I) :- heap(H, _).

swap(H, I, Ip) :-
    heap_entry(H, I, K, V),
    heap_entry(H, Ip, Kp, Vp),

    retract(heap_entry(H, I, K, V)),
    retract(heap_entry(H, Ip, Kp, Vp)),
    
    asserta(heap_entry(H, Ip, K, V)),
    asserta(heap_entry(H, I, Kp, Vp)).


 



% TEST
:- initialization(
    (
        
        assert(test_graph),
        assert(test_minheap),
        
        test_minheap -> (
            new_heap(a),
            insert(a, 2, x),
            insert(a, 1, y),
            insert(a, 3, z),
            insert(a, 5, m),
            insert(a, 4, n),
            insert(a, 4, p),
            list_heap(a),
            extract(a, 1, y),
            list_heap(a)
        ),

        test_graph -> (
            new_graph(g),
            new_vertex(g, a),
            new_vertex(g, b),
            new_vertex(g, c),
            new_vertex(g, d),
            new_vertex(g, e),
            list_vertices(g),

            new_edge(g, vertex(g, a), vertex(g, b), 3),
            new_edge(g, vertex(g, a), vertex(g, e), 11),
            new_edge(g, vertex(g, a), vertex(g, d), 4),
            new_edge(g, vertex(g, b), vertex(g, c), 5),
            new_edge(g, vertex(g, d), vertex(g, c), 7),
            new_edge(g, vertex(g, e), vertex(g, d), 6),
            list_edges(g)
        )
        /*
        init_sssp(g, vertex(g,a)),
        dijkstra(g, vertex(g,a)),
        forall(
            distance(g, V, C),
            (
                write(distance(g, V, C)),
                nl
            )
        ),
        nl,
        dijkstra(g, vertex(g,b)),
        forall(
            distance(g, V, C),
            (
                write(distance(g, V, C)),
                nl
            )
        )
        */
        /*
        dijkstra_sssp(g, vertex(g, a)),
        forall(
            distance(g, V, C),
            (
                write(distance(g, V, C)),
                nl
            )
        )
        */
    )
).