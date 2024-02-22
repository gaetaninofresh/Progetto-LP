Gaetano La Rocca, matricola 895887
Filippo Gentili, matricola 899906
Luca Giandomenico, matricola 900162



**** INTERFACCIA PROLOG PER LA MANIPOLAZIONE DEI GRAFI ****

Ogni grafo è definito da una serie di fatti salvati nel dataset:

:- new_graph(G).
Questo predicato inserisce un nuovo grafo nella base-dati Prolog. 


:- delete_graph(G).
Rimuove tutto il grafo (vertici e archi inclusi) dalla base-dati Prolog.


:- new_vertex(G, V).
Aggiunge il vertice V nella base-dati Prolog. N.B. si richiede che il predicato
che rappresenta i vertici, da aggiungere alla base-dati Prolog, sia 
vertex(G, V). 


:- vertices (G, Vs).
Questo predicato è vero quando Vs è una lista contenente tutti i vertici di G.
Nell'implementazione di questo predicato abbiamo dovuto aggiungere il predicato 
abolish/1 in quanto vertices/2 esisteva già nella libreria ugraph di Prolog, 
creando notevoli problemi. 


:- list_vertices(G).
Questo predicato stampa alla console dell’interprete Prolog una lista dei 
vertici del grafo G (usate listing/1).


:- new_edge(G, U, V, Weight).
Aggiunge un arco del grafo G alla base dati Prolog. N.B. è richiesto che il 
predicato che rappresenta gli archi, da aggiungere alla base-dati Prolog, 
sia arc(G, U, V, Weight). Per comodità potete anche costruire una versione 
new_arc/3 così definita: new_arc(G, U, V) :- new_arc(G, U, V, 1).


:- arcs(G, Es).
Questo predicato è vero quando Es è una lista di tutti gli archi presenti in G.


:- neighbors(G, V, Ns).
Questo predicato è vero quando V è un vertice di G e Ns è una lista contenente
gli archi, arc(G, V, N, W) e arc(G, N, V, W), che portano ai vertici N
immediatamente raggiungibili da V .


:- list_arcs(G).
Questo predicato stampa alla console dell’interprete Prolog una lista degli 
archi del grafo G (è il simmetrico di list_vertices/1).


:- list_graph(G).
Questo predicato stampa alla console dell’interprete Prolog una lista dei 
vertici e degli archi del grafo G.


:- vertex_neighbors(G, V, VNs).
Questo predicato controlla se G è un grafo e se V è un vertice di G, inserisce
un una lista tutti i vertici "vicini" a V, e poi da questa lista rimuove i 
vertici già visitati 
(quindi con distanza già calcolata e minima dalla sorgente).



I predicati implementati dall'API sono i seguenti:

new_graph(G)  ->  Inserisce il grafico G nel database prolog.

delete(G)  ->  rimuove il grafo G e tutti i suoi vertici e archi dal
     database prolog.

add_vertex(G, V)  ->  Aggiunge il vertice V del grafo G al database.

vertices(G, Vs) - >  Questo predicato è vero quanto Vs è una lista contenente 
    tutti i vertici di G .

list_vertices(G) ->  Questo predicato stampa alla console dell’interprete 
    Prolog una lista dei vertici del grafo G 

new_edge(G, V, U, Weight) -> Aggiunge un arco del grafo G alla base dati 
    Prolog, con peso non negativo.

edges(G, Es) -> Questo predicato è vero quando Es è una lista di tutti 
    gli archi presenti in G.

neighbors(G, V, Ns) ->  Questo predicato è vero quando V è un vertice di G e 
    Ns è una lista contenente gli archi edge(G, V, N, W)
    che portano ai vertici N immediatamente raggiungibili da V .

list_edges(G) ->  Questo predicato stampa alla console dell’interprete 
    Prolog una lista degli archi del grafo G.

list_graph(G)  ->  Questo predicato stampa alla console dell’interprete 
    Prolog una lista dei vertici e degli archi del grafo G.



**** MINHEAP IN PROLOG ****

I predicati implementati nell'API sono i seguenti:

new_heap(H)  ->  Questo predicato inserisce un nuovo heap 
    H nella base-dati Prolog.

delete_heap(H)  ->  Rimuove tutto lo heap (incluse tutte le “entries”) 
    dalla base-dati Prolog.

heap_size(H, S) ->  Questo predicato è vero quanto S è la 
    dimensione corrente dello heap.

empty(H) -> Questo predicato è vero quando lo heap H è vuoto.

not_empty(H) ->  Questo predicato è vero quando lo heap H non è vuoto.

head(H, K, V) ->  Il predicato è vero quando l’elemento dello heap H con
    chiave minima K è V.

insert(H, K, V) -> Il predicato è vero quando l’elemento V è inserito 
    nello heap H con chiave K.

extract(H, K, V) ->  Il predicato è vero quando la coppia K, V con K minima, 
    è rimossa dallo heap H.

modify_key(H, NewKey, oldKey, V) ->  Il predicato è vero quando 
    la chiave OldKey (associata al valore V) è sostituita da NewKey. 

list_heap(H) ->  Questo predicato stampa alla console dell’interprete 
    Prolog tutte le heap_entry dell'heap H.



**** SSSP IN PROLOG ****

visited(G, V) ->  Questo predicato è vero quando V è un vertice di G e, 
    durante e dopo l’esecuzione dell’algoritmo di Dijkstra, 
    V risulta visitato.

distance(G, V, D) ->  Questo predicato è vero quando V è un vertice di G e,
    durante e dopo l’esecuzione dell’algoritmo di Dijkstra, 
    la distanza minima del vertice V dalla sorgente è D.

previous(G, V, U) ->  Questo predicato è vero quando V ed U sono vertici 
    di G e, durante e dopo l’esecuzione dell’algoritmo di Dijkstra, 
    il vertice U è il vertice “precedente” a V nel cammino minimo
    dalla sorgente a V.

I predicati implementati nell'API sono i seguenti:

change_distance(G, V, NewDist) ->  Questo predicato ha sempre successo con 
    due effetti collaterali: prima tutte le istanze di dist(G, V, _) 
    sono ritirate dalla base-dati Prolog, e quindi dist(G, V, NewDist) 
    è asserita.

change_previous(G, V, U) ->  Questo predicato ha successo con due 
    effetti collaterali: prima tutte le istanze di previous(G, V, _) sono 
    ritirate dalla base-dati Prolog, e quindi previous(G, V, U) è asserita.


dijkstra_sssp(G, Source) ->  Questo predicato ha successo con 
    un effetto collaterale. Dopo la sua prova, la base-dati Prolog 
    ha al suo interno i predicati dist(G, V, D) per ogni V appartenente a G;
    la base-dati Prolog contiene anche i predicati previous(G, V, U) e 
    visited(V) per ogni V, ottenuti durante le iterazioni 
    dell’algoritmo di Dijkstra. 

shortest_path(G, Source, V, Path) ->  Questo predicato è vero quando 
    Path è una lista di archi.

**** PREDICATI AGGIUNTIVI ****

heapify(H, S) -> questo predicato organizza a partire dalla posizione P 
    gli elementi dell'heap in modo che, per qualsiasi indice i 
    considerato come genitore, i valori dei nodi figli agli indici 2i+1 
    e 2i+2 rispettano la proprietà di MAX-HEAP 

dijkstra(G, Source, Heap) -> questo predicato esegue l'aggiornamento dei costi
 dei nodi adiacenti ai nodi visitati, calcola i costi dei percorsi fino
 ad ora scoperti e li aggiunge all'heap di Dijkstra
 per l'elaborazione successiva

dijkstra_check_cost(G, V, Prev, R) -> Questo predicato è utilizzato 
    nell'algoritmo di Dijkstra per verificare se il percorso 
    attualmente considerato per un nodo V da un nodo precedente Prev
    ha un costo inferiore al costo attuale memorizzato per V 
    nel grafo G. In caso affermativo, il costo viene aggiornato.
