% per evitare warning
:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic edge/4.
:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic visited/2.
:- dynamic distance/3.
:- dynamic previous/3.



% nodo foglia
heapify(H, Node) :-
    heap_entry(H, Node, _, _),
    heap_size(H, Size),
    left_child(Node, Left),
    Left > Size, !.

% figlio unico - figlio >= padre
heapify(H, Node) :-
    heap_entry(H, Node, NodeK, _),
    heap_size(H, Size),
    left_child(Node, Left),
    Left = Size,
    heap_entry(H, Left, LeftK, _),
    LeftK >= NodeK, !.

% singolo figlio - figlio < padre
heapify(H, Node) :-
    heap_entry(H, Node, NodeK, _),
    heap_size(H, Size),
    left_child(Node, Left),
    Left = Size,
    heap_entry(H, Left, LeftK, _),
    LeftK < NodeK,
    swap(H, Node, Left),
    heapify(H, Left), !.

% due figli - figli >= padre
heapify(H, Node) :-
    heap_entry(H, Node, NodeK, _),
    left_child(Node, Left),
    heap_entry(H, Left, LeftK, _),
    LeftK >= NodeK,
    right_child(Node, Right),
    heap_entry(H, Right, RightK, _),
    RightK >= NodeK, !.

% due figli - figlio 1 >= padre e figlio 2 < padre
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

% due figli - figlio 1 < padre e figlio 2 >= padre
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

% due figli - figlio 1 < padre e figlio 2 < padre (figlio 1 =< figlio 2)
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

 % due figli - figlio 1 < padre e figlio 2 < padre (figlio 2 < figlio 1)
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
% nello heap H con chiave K; DEPRECATED(nel caso in cui la chiave sia già presente 
% sostituisce il vecchio valore), non fa nulla se proviamo a reinserire la 
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
    
    get_parent_index(NewS, P),
    heapify(H, P). 

% extract(H, K, V) - è vero quando la coppia K, V con K minima, è rimossa 
% dallo heap H
extract(H, _, _) :-
    empty(H).

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



    get_parent_index(I, Pi) :-
        I =< 1,
        Pi = 1, !.
    
    get_parent_index(I, Pi) :-
        I > 1,
        Pi is I div 2.

        swap(H, I, I) :- heap(H, _).

        swap(H, I, Ip) :-
            heap_entry(H, I, K, V),
            heap_entry(H, Ip, Kp, Vp),
        
            retract(heap_entry(H, I, K, V)),
            retract(heap_entry(H, Ip, Kp, Vp)),
            
            asserta(heap_entry(H, Ip, K, V)),
            asserta(heap_entry(H, I, Kp, Vp)).

            left_child(Pi, I) :-
                I is 2 * Pi.
            
            right_child(Pi, I) :-
                I is 2 * Pi + 1.


    :- initialization (
        (
            new_heap(h),
            insert(h, 4, a),
            insert(h, 2, b),
            insert(h, 7, c),
            insert(h, 9, d),
            insert(h, 3, e),
            insert(h, 5, f),
            insert(h, 1, g),
            insert(h, 5, i),
            list_heap(h)
            )
        ).