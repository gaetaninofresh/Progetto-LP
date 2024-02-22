Gaetano La Rocca, matricola 895887
Filippo Gentili, matricola 899906
Luca Giandomenico, matricola 900162

Funzioni aggiuntive lisp

dijkstra (graph-id vertex-id) -> esegue l'aggiornamento dei costi dei nodi 
adiacenti ai nodi visitati, calcola i costi dei percorsi 
fino ad ora scoperti e li aggiunge all'heap di Dijkstra per l'elaborazione successiva

sssp-shortest-path-function (graph-id source vertex-id path) -> ritorna il cammino 
minore tra la radice e un nodo

get-edge-cost (graph-id prev-id vertex-id) -> ritorna il costo di un arco tra due vertici

dijkstra-update (graph-id vertex-id to-explore-list) -> si occupa di aggiornare 
costi e predecessori dei nodi secondo l'algoritmo di dijkstra

dijkstra-heap-insert (heap-id vertex-id distance) -> chiama heap-insert o 
modify-key se l'elemento è presente o meno nell'heap

swap (heap-id index1 index2) -> inverte la posizione di due elementi in un array

heapify (heap-id index) -> questo predicato organizza a partire dalla radice
 gli elementi di un albero binario memorizzati in un array in modo tale che,
 per qualsiasi nodo considerato come genitore, i valori dei nodi 
figli siano maggiori del nodo padre

find-node (heap-id key value index &optional (check-key T)) -> effettua una ricerca
 lineare per trovare l'indice di un nodo nell'array

heapify-up (heap-id index) -> effettua l'heapify dal basso verso l'alto
