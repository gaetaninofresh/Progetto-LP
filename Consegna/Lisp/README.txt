Gaetano La Rocca, matricola 895887
Filippo Gentili, matricola 899906
Luca Giandomenico, matricola 900162

**** INTERFACCIA PER LA MANIPOLAZIONE DEI GRAFI ****

is-graph (graph-id) -> graph-id or NIL 
    Questa funzione ritorna il graph-id stesso se questo grafo è già stato 
    creato, oppure NIL se no. 

(new-graph graph-id) -> graph-id 
    Questa funzione genera un nuovo grafo e lo
    inserisce nel data base (ovvero nella hash-table) dei grafi. 

(delete-graph graph-id) -> NIL 
    Rimuove l’intero grafo dal sistema (vertici archi etc); ovvero 
    rimuove tutte le istanze presenti nei data base (ovvero nelle hash-tables)
    del sistema.

(new-vertex graph-id vertex-id) -> vertex-rep
    Aggiunge un nuovo vertice vertex-id al grafo graph-id. 
    
(graph-vertices graph-id) -> vertex-rep-list 
    Questa funzione torna una lista di vertici del grafo.

(new-edge graph-id vertex-id vertex-id &optional weight) -> edges-rep 
    Questa funzione aggiunge un arco del grafo graph-id nella 
    hash-table *edges*. La rappresentazione di un arco è 
    (edge graph-id u v weight)

(graph-edges graph-id) -> edge-rep-list 
Questa funzione ritorna una lista una lista di tutti gli archi presenti 
in graph-id.

(graph-vertex-neighbors graph-id vertex-id) -> vertex-rep-list 
Questa funzione ritorna una lista vertex-rep-list contenente gli archi,
(edge graph-id vertex-id N W), che portano ai vertici N immediatamente 
raggiungibili da vertex-id.

(graph-print graph-id) 
Questa funzione stampa alla console dell’interprete Common Lisp una lista
dei vertici e degli archi del grafo graph-id.



**** MINHEAP IN COMMON LISP ****

(new-heap H &optional capacity) -> heap-rep 
Questa funzione inserisce un nuovo heap nella hash-table *heaps*. 

(heap-delete heap-id) -> T 
Rimuove tutto lo heap indicizzato da heap-id. 

(heap-empty heap-id) -> boolean 
Questo predicato è vero quando lo heap heap-id non contiene elementi.

(heap-not-empty heap-id) -> boolean 
Questo predicato è vero quando lo heap heap-id contiene almeno un elemento.

(heap-head heap-id) -> (K V) 
La funzione heap-head ritorna una lista di due elementi dove K è la chiave 
minima e V il valore associato.

(heap-insert heap-id K V) -> boolean 
La funzione heap-insert inserisce l’elemento V nello heap heap-id con 
chiave K.

(heap-extract heap-id) -> (K V) 
La funzione heap-extract ritorna la lista con K, V e con K minima; la coppia 
è rimossa dallo heap heap-id. 

(heap-modify-key heap-id new-key old-key V) -> boolean 
La funzone heap-modify-key sostituisce la chiave OldKey (associata al 
valore V) con NewKey.

(heap-print heap-id) -> boolean
Questa funzione stampa sulla console lo stato interno dello heap heap-id.



**** SSSP IN COMMON LISP ****

(sssp-dist graph-id vertex-id) -> d 
Questa funzione, dato un vertex-id di un grafo graph-id ritorna, durante e 
dopo l’esecuzione dell’algoritmo di Dijkstra, la distanza minima d del vertice 
vertex-id dalla “sorgente”.


(sssp-visited graph-id vertex-id) -> boolean 
Questo predicato è vero quando vertex-id è un vertice di graph-id e, durante e
dopo l’esecuzione dell’algoritmo di Dijkstra, vertex-id risulta “visitato”.


(sssp-previous graph-id V) -> U 
Questa funzione, durante e dopo l’esecuzione dell’algoritmo di Dijkstra, 
ritorna il vertice U che è il vertice “precedente” a V nel cammino minimo 
dalla “sorgente” a V.


(sssp-change-dist graph-id V new-dist) -> NIL 
Questa funzione ha solo un effetto collaterale: alla chiave (graph-id V) 
nella hash-table *dist* viene associato il valore new-dist.


(sssp-change-previous graph-id V U) -> NIL 
Questa funzione ha solo un effetto collaterale: alla chiave (graph-id V) 
nella hash-table *previous* viene associato il valore U.


(sssp-dijkstra g graph-id source-id) -> NIL 
Questa funzione termina con un effetto collaterale. Dopo la sua esecuzione, 
la hash-table *distances* contiene al suo interno le associazioni 
(graph-id V) ⇒ d per ogni V appartenente a graph-id.

(sssp-shortest-path G Source V) -> Path 
Questa funzione ritorna una lista di archi che rappresenta il “cammino minimo” 
da Source a V.



**** FUNZIONI AGGIUNTIVE ****

(dijkstra graph-id vertex-id) -> NIL
    esegue l'aggiornamento dei costi dei nodi 
    adiacenti ai nodi visitati, calcola i costi dei percorsi 
    fino ad ora scoperti e li aggiunge all'heap di Dijkstra per l'elaborazione 
    successiva.

(sssp-shortest-path-function graph-id source vertex-id path) -> path
    ritorna la lista di archi che porta dal vertice source al vertice
    vertex-id.

(get-edge-cost graph-id prev-id vertex-id) -> cost
    ritorna il costo di un arco tra due vertici.

(dijkstra-update graph-id vertex-id to-explore-list) -> NIL
    si occupa di aggiornare costi e predecessori dei nodi 
    secondo l'algoritmo di dijkstra.

dijkstra-heap-insert (heap-id vertex-id distance) -> NIL
    chiama heap-insert o modify-key se l'elemento è presente o meno nell'heap.

heapify (heap-id index) -> NIL
    questo predicato organizza a partire dalla radice
    gli elementi di un albero binario memorizzati in un array in modo tale 
    che, per qualsiasi nodo considerato come genitore, i valori dei nodi 
    figli siano maggiori del nodo padre.

find-node (heap-id key value index &optional (check-key T)) -> index
    effettua una ricerca lineare per trovare 
    l'indice di un nodo nell'array.

heapify-up (heap-id index) -> NIL
    effettua l'heapify dal basso verso l'alto.
