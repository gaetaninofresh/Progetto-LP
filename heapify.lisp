(defun heapify (heap-id index)
    (let 
        (
            (node (aref (get-actual-heap heap-id) index))
            (left (aref (get-actual-heap heap-id) (left-child index)))
            (right (aref (get-actual-heap heap-id) (right-child index)))
        )
        
        ;caso base: il nodo è una foglia
        (if (and (nil left) (nil right))
            T
        )

        ;caso solo figlio sinistro
        (if (nil right)
            (if (> (first node) (first left))
                (
                    (swap heap-id index (left-child index))
                    (heapify heap-id (left-child index))
                )
            )
        )

        ;caso figlio destro minore 
        (if(and(not(nil right))(not(nil left)))
            (if(and(<= (first node) (first left))(> (first node) (first right)))
                (
                    (swap heap-id index (right-child index))
                    (heapify heap-id (right-child index))
                )
            )
        )

        ;caso figlio sinistro minore
        (if(and(not(nil right))(not(nil left)))
            (if(and(<= (first node) (first right))(> (first node) (first left)))
                (
                    (swap heap-id index (left-child index))
                    (heapify heap-id (left-child index))
                )
            )
        )

        ; caso entrambi minori
        (if(and(not(nil right))(not(nil left)))
            (if(and(> (first node) (first left))(> (first node) (first right)))
                (if(> (first right) (first left))
                    (
                        (swap head-id index (left-child index))
                        (heapify heap-id (left-child index))
                    )
                   ;else
                    (
                        (swap head-id index (right-child index))
                        (heapify heap-id (right-child index))
                    )
                )
            )
        )
    )
)


(defun heapify-up (heap-id index)
    
    (let (
            (node (aref (get-actual-heap heapid) index))
            (parent (aref (get-actual-heap heapid) (get-parent-index index)))
        )
        ;caso base
        (if (or (= index 0) (>= (first node) (first parent)))
            T
            ;else
            (
                (swap heap-id index (get-parent-index index))
                (heapify-up heap-id (get-parent-index index))
            )
        )

    )
)


#|
(defun heapify (heap-id index)
    (let 
        (
            (node (aref (get-actual-heap heap-id) index))
            (left (aref (get-actual-heap heap-id) (left-child index)))
            (right (aref (get-actual-heap heap-id) (right-child index)))
        )
        
        ;caso base: il nodo è una foglia
        (if (and (null left) (null right))
            T
        )

        ;caso solo figlio sinistro
        (if (null right)
              (if (> (first node) (first left))
                (progn
                    (swap heap-id index (left-child index))
                    (heapify heap-id (left-child index))
                )
              )
        )

        ;caso figlio destro minore 
        (if(and(not(null right))(not(null left)))
            (if(and(<= (first node) (first left))(> (first node) (first right)))
                (progn
                    (swap heap-id index (right-child index))
                    (heapify heap-id (right-child index))
                )
            )
        )

        ;caso figlio sinistro minore
        (if(and(not(null right))(not(null left)))
            (if(and(<= (first node) (first right))(> (first node) (first left)))
                (progn
                    (swap heap-id index (left-child index))
                    (heapify heap-id (left-child index))
                )
            )
        )

        ; caso entrambi minori
        (if(and(not(null right))(not(null left)))
            (if(and(> (first node) (first left))(> (first node) (first right)))
                (if(> (first right) (first left))
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

|#



(defun test (heap-id)
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
