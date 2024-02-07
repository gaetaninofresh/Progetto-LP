(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *edges* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))

(defun is-graph (graph-id)
 ;; graph-id è un atomo: un simbolo (non NIL) o un intero.
 (gethash graph-id *graphs*))

(defun new-graph (graph-id)
 ;; graph-id è un atomo: un simbolo (non NIL) o un intero.
 (or (gethash graph-id *graphs*)
 (setf (gethash graph-id *graphs*) graph-id)))

(defun delete-graph (graph-id)
  ;; Rimuove l'intero grafo dal sistema
  (remhash graph-id *vertices*)
  (remhash graph-id *graphs*)
  (remhash graph-id *edges*)
  NIL)

(defun new-vertex (graph-id vertex-id)
 ;; graph-id è un atomo: un simbolo (non NIL) o un intero.
 ;; vertex-id pure.
 (setf (gethash (list 'vertex graph-id vertex-id)*vertices*))
 (list 'vertex graph-id vertex-id))

(defun graph-vertices (graph-id)
  ;; Ottiene il grafo dalla hash-table *graphs*
  (let ((graph (gethash graph-id *graphs*)))
    ;; Se il grafo non esiste, restituisce NIL
    (if (not graph)
        NIL
      ;; Altrimenti, restituisce la lista dei vertici del grafo
      ;; (Assumendo che il grafo sia rappresentato come una lista di vertici)
      (mapcar #'vertex-id graph))))

(defun new-edge (graph-id u v &optional (weight 0))
  ;; Controlla se il grafo esiste
  (if (not (gethash graph-id *graphs*))
      (error "Il grafo non esiste.")
    ;; Controlla se i vertici esistono
    (if (or (not (gethash u *vertices*)) (not (gethash v *vertices*)))
        (error "Uno o entrambi i vertici non esistono.")
      ;; Se il grafo e i vertici esistono, crea una rappresentazione dell'arco
      (let ((edge (list 'edge graph-id u v weight)))
        ;; Aggiunge l'arco alla hash-table *edges*
        (setf (gethash (list graph-id u v) *edges*) edge)
        ;; Restituisce la rappresentazione dell'arco
        edge))))

(defun graph-edges (graph-id)
  ;; Ottiene il grafo dalla hash-table *graphs*
  (let ((graph (gethash graph-id *graphs*)))
    ;; Se il grafo non esiste, restituisce NIL
    (if (not graph)
        NIL
      ;; Altrimenti, restituisce la lista degli archi del grafo
      ;; (Assumendo che il grafo sia rappresentato come una lista di archi)
      (mapcar #'edge-id graph))))

(defun graph-vertex-neighbors (graph-id vertex-id)
  (let ((graph (gethash graph-id *graphs*)))
    (if (not graph)
        NIL
      ;; Altrimenti, restituisce la lista degli archi che partono da vertex-id
      (let ((edges (gethash vertex-id *edges*)))
        ;; Se non ci sono archi che partono da vertex-id, restituisce NIL
        (if (not edges)
            NIL
          ;; Altrimenti, filtra la lista degli archi per includere solo quelli
          ;; che partono da vertex-id
          (remove-if-not (lambda (edge) 
            (equal (second edge) vertex-id)) edges))))))


;; CODICE LUCA
;Questa funzione ritorna il graph-id stesso.
(defun is-graph (graph-id)
  (gethash graph-id *graphs*))

;Questa funzione genera un nuovo grafo e lo inserisce nel data base
(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
      (setf 
       (gethash graph-id *graphs*) 
       graph-id)))

;Rimuove l'intero grafo dal sistema 
(defun delete-graph (graph-id)
  (remhash graph-id *graphs*)
  nil)

;Aggiunge un nuovo vertice vertex-id al grafo graph-id.
(defun new-vertex (graph-id vertex-id) 
  (setf (gethash (list vertex graph-id vertex-id) *vertices*)
        (list vertex graph-id vertex-id)))

;Questa funzione torna una lista di vertici del grafo.


