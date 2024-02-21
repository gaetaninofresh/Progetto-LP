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

;; Migliorabile esplorando l'heap 
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

;;; TEST

(defun test-1 (graph-id)
  (new-graph graph-id)
  (new-vertex graph-id 's)
  (new-vertex graph-id 'a)
  (new-vertex graph-id 'b)
  (new-vertex graph-id 'c)
  (new-vertex graph-id 'd)
  (new-vertex graph-id 'e)
  (new-vertex graph-id 'f)
  ;nodo separato
  (new-vertex graph-id 'h)

  (new-edge graph-id 'a 'b 6)
  (new-edge graph-id 's 'a 2)
  (new-edge graph-id 's 'd 8)
  (new-edge graph-id 'a 'c 2)
  (new-edge graph-id 'd 'c 2)
  (new-edge graph-id 'd 'e 2)
  (new-edge graph-id 'c 'e 9)
  (new-edge graph-id 'e 'f 1)
  (new-edge graph-id 'b 'f 5)

  (graph-print graph-id)
  T
)

(defun test-3 (graph-id source vertex)
  (test-1 graph-id)
  (sssp-dijkstra graph-id source)
  (sssp-shortest-path-function graph-id source vertex ())
  
)

(defun test-4 (graph-id source vertex)
    (new-graph graph-id)
    (new-vertex graph-id 's)
    (new-vertex graph-id 'a)
    (new-vertex graph-id 'b)
    (new-vertex graph-id 'c)
    (new-edge graph-id 's 'a )
    (new-edge graph-id 's 'b )
    (new-edge graph-id 'a 'c )
    (new-edge graph-id 'b 's )
    (sssp-dijkstra graph-id source)
    (sssp-shortest-path graph-id source vertex)
  
)

(defun test-heap (heap-id)
    (new-heap heap-id 3)
    (heap-insert heap-id '1 'a)
    (heap-insert heap-id '2 'b)
    (heap-insert heap-id '3 'c)
    (heap-insert heap-id '4 'd)
)


(defun test-big (graph-id source)
    (new-graph graph-id)
    (new-vertex graph-id 's)
    (new-vertex graph-id 'a)
    (new-vertex graph-id 'b)
    (new-vertex graph-id 'c)
    (new-vertex graph-id 'd)
    (new-vertex graph-id 'e)
    (new-vertex graph-id 'f)
    (new-vertex graph-id '1)
    (new-vertex graph-id '2)
    (new-vertex graph-id '3)
    (new-vertex graph-id '4)
    (new-vertex graph-id '5)
    (new-vertex graph-id '6)
    (new-vertex graph-id '7)
    (new-vertex graph-id '8)
    (new-vertex graph-id '9)
    (new-vertex graph-id '0)
    (new-vertex graph-id 'g)
    (new-vertex graph-id 'h)
    (new-vertex graph-id 'i)
    (new-vertex graph-id 'j)
    (new-vertex graph-id 'k)
    (new-vertex graph-id 'l)
    (new-vertex graph-id 'm)
    (new-vertex graph-id 'n)
    (new-vertex graph-id 'o)
    (new-vertex graph-id 'p)
    (new-vertex graph-id 'q)
    (new-vertex graph-id 'r)
    (new-vertex graph-id 't)
    (new-vertex graph-id 'u)
    (new-vertex graph-id 'v)
    (new-vertex graph-id 'w)
    (new-vertex graph-id 'x)
    (new-vertex graph-id 'y)
    (new-vertex graph-id 'z)

    (new-edge graph-id 'a 'b 6)
    (new-edge graph-id 's 'a 2)
    (new-edge graph-id 's 'd 8)
    (new-edge graph-id 'a 'c 2)
    (new-edge graph-id 'd 'c 2)
    (new-edge graph-id 'd 'e 2)
    (new-edge graph-id 'c 'e 9)
    (new-edge graph-id 'e 'f 1)
    (new-edge graph-id 'b 'f 5)
    (new-edge graph-id '1 '2 3)
    (new-edge graph-id '3 '4 7)
    (new-edge graph-id '5 '6 4)
    (new-edge graph-id '7 '8 1)
    (new-edge graph-id '9 '0 5)
    (new-edge graph-id 'g 'h 3)
    (new-edge graph-id 'i 'j 6)
    (new-edge graph-id 'k 'l 2)
    (new-edge graph-id 'm 'n 9)
    (new-edge graph-id 'o 'p 1)
    (new-edge graph-id 'q 'r 5)
    (new-edge graph-id 't 'u 4)
    (new-edge graph-id 'v 'w 8)
    (new-edge graph-id 'x 'y 2)
    (new-edge graph-id 'z 's 3)
    (new-edge graph-id 'b 'z 7)
    (new-edge graph-id 'q 'l 6)
    (new-edge graph-id 'e 'o 4)
    (new-edge graph-id 'k 'u 2)
    (new-edge graph-id 'p 'x 9)
    (new-edge graph-id 'y 'f 1)
    (new-edge graph-id 'c 'v 5)
    (new-edge graph-id 'r 'd 8)
    (new-edge graph-id 'n 'w 7)
    (new-edge graph-id 'g 't 3)
    (new-edge graph-id 'h 'i 6)
    (new-edge graph-id 'j 'y 2)
    (new-edge graph-id 'm 'x 1)
    (new-edge graph-id 'r 'z 5)
    (new-edge graph-id 'o 'q 4)
    (new-edge graph-id 'p 'n 8)
    (new-edge graph-id 'u 'v 3)
    (new-edge graph-id 'w 'l 6)
    (new-edge graph-id 'k 'c 4)
    (new-edge graph-id 's 'h 2)
    (new-edge graph-id 'b 'g 7)
    (new-edge graph-id 't 'j 9)
    (new-edge graph-id 'k 'r 1)
    (new-edge graph-id 'm 'e 3)
    (new-edge graph-id 'o 'f 2)
    (new-edge graph-id 'l 'd 6)
    (new-edge graph-id 's 'q 5)
    (new-edge graph-id 'z 'u 1)
    (new-edge graph-id 'n 'i 4)
    (new-edge graph-id 'b 'w 8)
    (new-edge graph-id 'a 'x 7)
    (new-edge graph-id 'y 'v 2)
    (new-edge graph-id 't 'g 9)
    (new-edge graph-id 'k 's 3)
    (new-edge graph-id 'e 'y 6)
    (new-edge graph-id 'j 'h 4)
    (new-edge graph-id 'r 'n 7)
    (new-edge graph-id 'c 'l 5)
    (new-edge graph-id 'q 'f 2)
    (new-edge graph-id 'p 'b 9)
    (new-edge graph-id 'm 'u 3)
    (new-edge graph-id 'o 'a 6)
    (new-edge graph-id 'i 'z 4)
    (new-edge graph-id 'd 't 8)
    (new-edge graph-id 'e 'v 7)
    (new-edge graph-id 'w 'y 2)
    (new-edge graph-id 'x 'r 5)
    (new-edge graph-id '1 'o 1)
    (new-edge graph-id '2 'i 9)
    (new-edge graph-id '3 'u 8)
    (new-edge graph-id '4 'p 4)
    (new-edge graph-id '5 'm 3)
    (new-edge graph-id '6 'q 2)
    (new-edge graph-id '7 'n 6)
    (new-edge graph-id '8 'r 1)
    (new-edge graph-id '9 'k 5)
    (new-edge graph-id '0 's 7)

    (sssp-dijkstra graph-id source)

    (sssp-shortest-path graph-id 'a 'b)
    (sssp-shortest-path graph-id 's 'd)
    (sssp-shortest-path graph-id 'f 'g)
    (sssp-shortest-path graph-id '1 '2)
    (sssp-shortest-path graph-id '3 '4)
    (sssp-shortest-path graph-id '5 '6)
    (sssp-shortest-path graph-id '7 '8)
    (sssp-shortest-path graph-id '9 '0)
    (sssp-shortest-path graph-id 'm 'n)
    (sssp-shortest-path graph-id 'o 'p)
    (sssp-shortest-path graph-id 'q 'r)
    (sssp-shortest-path graph-id 't 'u)
    (sssp-shortest-path graph-id 'v 'w)
    (sssp-shortest-path graph-id 'x 'y)
    (sssp-shortest-path graph-id 'z 'a)
    (sssp-shortest-path graph-id 'l 'k)
    (sssp-shortest-path graph-id 'j 'i)
    (sssp-shortest-path graph-id 'h 'g)
    (sssp-shortest-path graph-id 'f 'e)
    (sssp-shortest-path graph-id 'd 'c)
)