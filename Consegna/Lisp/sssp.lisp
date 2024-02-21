;;; Definizione hashtables
(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *edges* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))

;;; is-graph - ritorna il graph-id stesso.
(defun is-graph (graph-id)
  (gethash graph-id *graphs*)
)

;;; new-graph - genera un nuovo grafo e lo inserisce nel data base
(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
      (setf 
       (gethash graph-id *graphs*) 
       graph-id)
    )
)

;;; delete-graph rimuove l'intero grafo dal sistema 
(defun delete-graph (graph-id)
    (remhash graph-id *graphs*) 
    nil
)

;;; new-vertex - aggiunge un nuovo vertice vertex-id al grafo graph-id.
(defun new-vertex (graph-id vertex-id) 
  (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
        (list 'vertex graph-id vertex-id)))

;;; graph-vertices - ritorna una lista di vertici del grafo.
(defun graph-vertices (graph-id)
  (let ((key-list ()) (value-list ()))
    (maphash 
        (lambda (key value)
            (cond (
                (equal (second key) graph-id)
                    (push key key-list)
                    (push value value-list)
                )
            )
        )
      *vertices*
    )
    value-list
  )
)

;;; new-edge - aggiunge un edge all'hashtable
(defun new-edge (graph-id vertex-id vertex2-id &optional (weight 1))
  ; controllo se esistano i veritici nel grafo
  (gethash graph-id *graphs*)
  (and (gethash vertex-id *vertices*) (equal (second vertex-id ) graph-id))
  (and (gethash vertex2-id *vertices*) (equal (second vertex2-id ) graph-id))

  (setf (gethash (list 'edge graph-id vertex-id vertex2-id weight) *edges*)
        (list 'edge graph-id vertex-id vertex2-id weight))
)

(defun graph-edges (graph-id)
    (let ((key-list ()) (edge-list ()))
        (maphash 
            (lambda (key edge)
                (cond ((equal (second edge) graph-id)
                        (push edge edge-list)
                        (push key key-list)
                    )
                )
            )
            *edges*
        )
        edge-list
    )
)

(defun graph-vertex-neighbors (graph-id vertex-id)
    (let ((key-list ()) (neigh-list ()))
        (maphash
            (lambda (key neigh)
                (cond ((and (equal (second neigh) graph-id) 
                            (equal (third neigh) vertex-id))
                        (push (fourth neigh) 
                            neigh-list)
                        (push key key-list)
                    )
                )
            )
            *edges*
        )
        neigh-list
    )
)

(defun graph-print (graph-id)
   (maphash 
        (lambda (key value)
            (and  
                (equal (second key) graph-id)
                (print value)
            )
        )
        *vertices*
    )
   (maphash 
        (lambda (key value)
            (and
                (equal (second key) graph-id)
                (print value)
            )
        )
        *edges*
    )
)

;;; MIN HEAP

(defun new-heap (heap-id &optional (capacity 42))
    (or (gethash heap-id *heaps*)
        (setf (gethash heap-id *heaps*)
            (list 'heap heap-id 0 (make-array capacity))
        )
    )
)

(defun get-heap-id (heap-rep)
    (second heap-rep)
)

(defun get-heap-size (heap-id)
    (third (gethash heap-id *heaps*))
)

(defun get-actual-heap (heap-id)
    (fourth (gethash heap-id *heaps*))
)

(defun heap-delete (heap-id)
    (remhash heap-id *heaps*)
)

(defun heap-empty (heap-id)
    (= (get-heap-size heap-id) 0)
)

(defun heap-not-empty (heap-id)
    (not (heap-empty heap-id))
)

(defun heap-head (heap-id) 
    (aref (get-actual-heap heap-id) 0)
)


(defun heap-insert (heap-id key value)
    ;; inserisce l'elemento in ultima posizione
    (if (< (array-total-size (get-actual-heap heap-id)) 
        (+ (get-heap-size heap-id) 1))
        (progn
            (format t 
                "~%You have exceeded the maximum ~
                heap size of ~S for heap ~S ~%"     
                (array-total-size (get-actual-heap heap-id))
                heap-id
            )
            nil
        )
        ;else
        (progn
            (setf (aref 
                    (get-actual-heap heap-id) 
                    (get-heap-size heap-id)
                )
                (list key value)
            )
            
            ;; aggiorna dimensione heap
            (setf (gethash heap-id *heaps*)
                (list
                    'heap
                    heap-id
                    (+ (get-heap-size heap-id) 1)
                    (get-actual-heap heap-id) 
                )
            )
            ;; chiama heapify   
            (heapify-up heap-id (- (get-heap-size heap-id) 1))
        )
    )
)

(defun heap-extract (heap-id)
    (let* ((head (heap-head heap-id)))
        (setf (aref (get-actual-heap heap-id) 0) NIL)
        (swap heap-id (- (get-heap-size heap-id) 1) 0)

        ;reduce heap size
        (setf (gethash heap-id *heaps*)
            (list
                'heap
                heap-id
                (- (get-heap-size heap-id) 1)
                (get-actual-heap heap-id) 
            )
        )
        ;chiamata heapify
        (if (> (get-heap-size heap-id) 0)
            (heapify-up heap-id (- (get-heap-size heap-id) 1))
            (heapify heap-id 0)
        )
        ;return
        head
    )
)

(defun heap-modify-key (heap-id new-key old-key value)
    (let ((index (find-node heap-id old-key value 0)))
        (if (null index)
            NIL
        ;else
            (progn
                (setf (aref (get-actual-heap heap-id) index) 
                (list new-key value))
                (heapify-up heap-id index)
                (heapify heap-id 0)
                T
            )
        )
    )
)

(defun heap-print (heap-id)
    (print (gethash heap-id *heaps*)) 
    t
)

;;; DIJKSTRA

(defun sssp-dist (graph-id vertex-id)
    (gethash (list graph-id vertex-id) *distances*)
)

(defun sssp-change-dist (graph-id vertex-id new-distance)
    (setf (gethash (list graph-id vertex-id) *distances*) 
        new-distance
    )
    NIL
)

(defun sssp-visited (graph-id vertex-id)
    (gethash (list graph-id vertex-id) *visited*)
)

(defun sssp-set-visited (graph-id vertex-id)
    (setf (gethash (list graph-id vertex-id) *visited*) T)
    NIL
)

(defun sssp-previous (graph-id vertex-id)
    (gethash (list graph-id vertex-id) *previous*)    
)

(defun sssp-change-previous (graph-id vertex-id previous-id)
    (setf (gethash (list graph-id vertex-id) *previous*) previous-id)
    NIL
)


(defun sssp-dijkstra (graph-id vertex-id)
    
    ;per evitare di usare un heap statico
    (heap-delete graph-id)
    (new-heap graph-id)
    
    ;pulizia hashtable
    (clrhash *visited*)
    (clrhash *distances*)
    (clrhash *previous*)
    
    (sssp-set-visited graph-id vertex-id)
    (sssp-change-dist graph-id vertex-id 0)
    (heap-insert graph-id 0 vertex-id)
    
    (dijkstra graph-id vertex-id)
    (heap-delete graph-id)
    NIL
    
)

(defun sssp-shortest-path (graph-id source-id vertex-id)
    (sssp-shortest-path-function graph-id source-id vertex-id ())
)


;;; FUNZIONI AGGIUNTIVE

(defun dijkstra (graph-id vertex-id)
    
    (if (heap-empty graph-id)
        T
        ;else
        (progn
            (sssp-set-visited graph-id vertex-id)
            (heap-extract graph-id)
            
            
            (maphash (lambda (key value)
                    (and 
                        (equal (first key) graph-id)
                        (equal value T)
                    )

                    (dijkstra-update graph-id (second key) 
                        (graph-vertex-neighbors graph-id (second key))
                    )
                )
                *visited*
            )
            
            #| 
            (format t "~% visited: ~%")
            (print-hash *visited*)
            (format t "~% distances: ~%")
            (print-hash *distances*)
            (format t "~% previous: ~%")
            (print-hash *previous*)

            (heap-print graph-id)
            |#
            
            
            
            (dijkstra graph-id (second (heap-head graph-id)))

        )
    )
)

(defun sssp-shortest-path-function (graph-id source vertex-id path)
  (let* 
        (
            (prev-id (sssp-previous graph-id vertex-id))
            (cost (get-edge-cost graph-id prev-id vertex-id)) 
        )
        
        (if (equal source vertex-id)            
            path
            (if (null prev-id)
                NIL
                ;else
                (progn 
                    (push (list 'edge graph-id prev-id vertex-id cost) path)
                    (sssp-shortest-path-function graph-id source prev-id path)
                )
            )
        )
    )
)

(defun get-edge-cost (graph-id prev-id vertex-id)
    (let (cost)
        (maphash (lambda (key value)
            (when (and 
                    (equal (second key) graph-id)
                    (equal (third key) prev-id)
                    (equal (fourth key) vertex-id)
                )

                (setf cost (fifth key))
            )
        )
            *edges*
        )
        cost
    )
)


(defun dijkstra-update (graph-id vertex-id to-explore-list)
   
   (if (null to-explore-list)
        T
        ;else  
        (progn 
            (let* (
                    (node-dist (gethash (list graph-id vertex-id) *distances*))
                    (neigh-id (first to-explore-list))
                    (arc-cost (get-edge-cost graph-id vertex-id neigh-id))
                    (old-dist (gethash (list graph-id neigh-id) *distances*))
                )

                ;caso nodo a distanza infinita
                (if (null old-dist)
                    (progn
                        (sssp-change-dist graph-id neigh-id 
                            (+ node-dist arc-cost)
                        )
                        (sssp-change-previous graph-id neigh-id vertex-id)

                        (dijkstra-heap-insert graph-id neigh-id 
                            (+ node-dist arc-cost)
                        )
                    )
                ;else
                    ;cammino migliore
                    (if (> old-dist (+ node-dist arc-cost))
                        (progn
                            (sssp-change-dist graph-id neigh-id 
                                (+ node-dist arc-cost)
                            )
                            (sssp-change-previous graph-id neigh-id vertex-id)

                            (dijkstra-heap-insert graph-id neigh-id 
                                (+ node-dist arc-cost)
                            )
                        )
                    )
                )

            )
            (dijkstra-update graph-id vertex-id (rest to-explore-list))
        )
    )
)

(defun dijkstra-heap-insert (heap-id vertex-id distance)
    (let (
            (key (find-node heap-id nil vertex-id 0)) 
        )  
        (if (null key)
            (heap-insert heap-id distance vertex-id)
            ;else
            (heap-modify-key heap-id distance key vertex-id)
        )
    )
)


(defun get-parent-index (index)
    (floor (/ index 2))
)

(defun left-child (index)
    (+ (* index 2) 1)
)

(defun right-child (index)
    (+ (* index 2) 2)
)

(defun swap (heap-id index1 index2)
    (let* (
            (heap-array (get-actual-heap heap-id))
            (element1 (aref heap-array index1))
            (element2 (aref heap-array index2))
        )
        (setf (aref heap-array index1) element2)
        (setf (aref heap-array index2) element1)
    )
    T
)

(defun heapify (heap-id index)
    (let
        (
            (left-index (left-child index))
            (right-index (right-child index))
        )

        (if (or (>= left-index (get-heap-size heap-id))
                    (>= right-index (get-heap-size heap-id)))
                NIL
            
            ;else
            (let
                (
                    (node (aref (get-actual-heap heap-id) index))
                    (left (aref (get-actual-heap heap-id)
                        (left-child index)))
                    (right (aref (get-actual-heap heap-id) 
                        (right-child index)))
                )

                ;caso base: il nodo è una foglia

                (if (null left)
                    T
                )


                ;caso solo figlio sinistro
                (if (and (not (null left)) (null right))
                    (if (> (first node) (first left))
                        (progn
                            (swap heap-id index (left-child index))
                            (heapify heap-id (left-child index))
                        )
                    )
                )

                ;caso figlio destro minore 
                (if (and (not (null right)) (not (null left)))
                    (if (and (<= (first node) (first left))
                            (> (first node) (first right))
                        )
                        (progn
                            (swap heap-id index (right-child index))
                            (heapify heap-id (right-child index))
                        )
                    )
                )

                ;caso figlio sinistro minore
                (if (and (not (null right))(not (null left)))
                    (if (and (<= (first node) (first right))
                        (> (first node) (first left)))
                        (progn
                            (swap heap-id index (left-child index))
                            (heapify heap-id (left-child index))
                        )
                    )
                )

                ; caso entrambi minori
                (if (and (not (null right)) (not (null left)))
                    (if (and (> (first node) (first left))
                            (> (first node) (first right)))
                        (if (> (first right) (first left))
                            (progn
                                (swap heap-id index (left-child index))
                                (heapify heap-id (left-child index))
                            )
                        ;else
                            (progn
                                (swap heap-id index (right-child index))
                                (heapify heap-id (right-child index))
                            )
                        )
                    )
                )
            )
        )
    )
)

(defun find-node (heap-id key value index &optional (check-key T))
    (if (> index (get-heap-size heap-id))
        NIL
        ;; else
        (if (and
                (or 
                    (equal key (first (aref (get-actual-heap heap-id) index)))
                    (not check-key)
                ) 
                (equal value (second (aref (get-actual-heap heap-id) index))) 
            )
            index

            ;; else
            (find-node heap-id key value (+ index 1))
        )
    )
)

(defun heapify-up (heap-id index)
    
    (let (
            (node (aref (get-actual-heap heap-id) index))
            (parent (aref (get-actual-heap heap-id) (get-parent-index index)))
        )
        ;caso base
        (if (or (= index 0) (>= (first node) (first parent)))
            T
            ;else
            (progn
                (swap heap-id index (get-parent-index index))
                (heapify-up heap-id (get-parent-index index))
            )
        )

    )
)

;; for debug purposes
(defun print-hash (hash)
    (maphash (lambda (key value)
        
            (format t "~S -> ~S ~%" key value)
        )
        hash
    )
)