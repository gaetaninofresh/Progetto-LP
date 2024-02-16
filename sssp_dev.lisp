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
  (gethash graph-id *graphs*))

;;; new-graph - genera un nuovo grafo e lo inserisce nel data base
(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
      (setf 
       (gethash graph-id *graphs*) 
       graph-id)))

;;; delete-graph rimuove l'intero grafo dal sistema 
(defun delete-graph (graph-id)
  (remhash graph-id *graphs*)
  nil)

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
(defun new-edge (graph-id vertex-id vertex2-id &optional weight)
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
                        (push (list 'vertex graph-id (fourth neigh)) 
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

(defun heap-print (heap-id)
  (print (gethash heap-id *heaps*)) t)



(defun heap-insert (heap-id key value)
    ;; inserisce l'elemento in ultima posizione
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
    ;; chiama heapify sull'ultimo elemento
    ;(heapify heap-id (- (get-heap-size heap-id) 1))
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
        (heapify heap-id (- (get-heap-size heap-id) 1))
        head
    )
)



;;; FUNZIONI AGGIUNTIVE

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
        (setf(aref heap-array index1) element2)
        (setf(aref heap-array index2) element1)
    )
    T
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
  (new-edge graph-id 'a 'b 6)
  (new-edge graph-id 's 'a 2)
  (new-edge graph-id 's 'd 8)
  (new-edge graph-id 'a 'c 2)
  (new-edge graph-id 'd 'c 2)
  (new-edge graph-id 'd 'e 2)
  (new-edge graph-id 'c 'e 9)
  (new-edge graph-id 'e 'f 1)
  (new-edge graph-id 'b 'f 5)
  T
)

(defun test-2 (heap-id)
  (new-heap heap-id 12)
  (heap-insert heap-id '4 'a)
  (heap-insert heap-id '2 'b)
  (heap-insert heap-id '7 'c)
  (heap-insert heap-id '9 'd)
  (heap-insert heap-id '3 'e)
  (heap-insert heap-id '5 'f)
  (heap-insert heap-id '1 'g)
  ;(heap-insert heap-id '5 'h)

  (print (get-actual-heap heap-id))
  T
)
