(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *edges* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))

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
                    (left (aref (get-actual-heap heap-id) (left-child index)))
                    (right (aref (get-actual-heap heap-id) (right-child index)))
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
  (print (gethash heap-id *heaps*)) 
  T
)


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
    ;; chiama heapify 
    (heapify-up heap-id (- (get-heap-size heap-id) 1))
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
        (heapify-up heap-id (- (get-heap-size heap-id) 1))
        (heapify heap-id 0)
        
        ;return
        head
    )
)

;; Migliorabile esplorando l'heap 
(defun find-node (heap-id key value index)
    (if (> index (get-heap-size heap-id))
        NIL
        ;; else
        (if (and
                (equal key (first (aref (get-actual-heap heap-id) index))) 
                (equal value (second (aref (get-actual-heap heap-id) index))) 
            )
            index

            ;; else
            (find-node heap-id key value (+ index 1))
        )
    )
)

(defun modify-key (heap-id new-key old-key value)
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



(defun test (heap-id)
  (new-heap heap-id 12)
  (heap-insert heap-id '4 'a)
  (heap-insert heap-id '2 'b)
  (heap-insert heap-id '7 'c)
  (heap-insert heap-id '9 'd)
  (heap-insert heap-id '3 'e)
  (heap-insert heap-id '5 'f)
  (heap-insert heap-id '1 'g)
  (heap-insert heap-id '5 'h)

  (print (get-actual-heap heap-id))
  T
)

(defun test-1 (heap-id)
  (heapify-up heap-id 6)
  (print (get-actual-heap heap-id))
  (heapify heap-id 1)
  (print (get-actual-heap heap-id))
  T
)
