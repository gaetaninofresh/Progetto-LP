% per evitare warning
:- dynamic graph/1.

% new_graph(G) - aggiunge il grafo alla base di conoscenza

new_graph(G) :- 
    graph(G), !.

new_graph(G) :-
    assert(graph(G)), !.

% delete_graph(G) - rimuove G dalla base di conoscenza

delete_graph(G) :-
    graph(G),
    retract(G), !.
    
    
