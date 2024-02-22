Gaetano La Rocca, matricola 895887
Filippo Gentili, matricola 899906
Luca Giandomenico, matricola 900162



**** INTERFACCIA PROLOG PER LA MANIPOLAZIONE DEI GRAFI ****

Ogni grafico è definito da una serie di fatti salvati nel dataset:

graph(G) -> rappresentazione nella base dati del grafo G

vertex(G, V) -> rappresentazione nella base dati del vertice V del grafo G

arc(G, V, U, Weight) -> rappresentazione nella base dati dell'arco tra i
     vertici V e U



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
