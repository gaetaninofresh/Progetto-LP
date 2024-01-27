% per evitare warning
:- dynamic graph/1.
:- dynamic vertex/2.

% new_graph(G) - aggiunge il grafo G alla base di conoscenza

new_graph(G) :- 
    graph(G), !.

new_graph(G) :-
    assert(graph(G)), !.

% delete_graph(G) - rimuove il grafo G dalla base di conoscenza

delete_graph(G) :-
    retract(graph(G)).
    
% new_vertex(G, V) - crea un nuovo vertice V nel grafo G   
new_vertex(G, V) :-
    graph(G),
    atom(V),
    not(vertex(G, V)),
    assert(vertex(G, V)).
