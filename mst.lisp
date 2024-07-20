;;;; -*- Mode: Common Lisp -*-
;;;; mst.lisp

;;;; MINIMUM SPANNING TREES

;;;; Componenti del gruppo

;;;; Studente Alessandro Biagiotti
;;;; Numero di matricola: 869014

;;;; Studente Youssef Benchaib
;;;; Numero di matricola: 844515

;;;; Studente Giacomo Elemi
;;;; Numero di matricola: 806904

;; Sezione di definizione delle hashtables

(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal)) 
(defparameter *positions* (make-hash-table :test #'equal))
(defparameter *vertex-keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *neighbors* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))

;;; SEZIONE DI CODIFICA DELL'INTERFACCIA
(defun new-graph (graph-id)
  (or
   (gethash graph-id *graphs*)
   (setf (gethash graph-id *graphs*)
	 graph-id)))


(defun is-graph (graph-id)
  (if
      (gethash graph-id *graphs*)

      graph-id
    nil))


(defun delete-graph (graph-id)
  (if
      (is-graph
       graph-id)

      ;; cancello il grafo
      (progn
	(remhash
	 graph-id
	 *graphs*)
	
	;; cancello i vertici
	(maphash
	 #'(lambda
	     (k v)
	     (if
		 (equal
		  (second v)
		  graph-id)
		 (remhash
		  k
		  *vertices*)))
	 *vertices*)
	
	;; cancello gli archi
        (maphash
	 #'(lambda
	     (k v)
	     (if
		 (equal
		  (second v)
		  graph-id)
		 (remhash
		  k
		  *arcs*)))
	 *arcs*)
	nil)
    
    (error "Il grafo non esiste")))


(defun new-vertex (graph-id vertex-id)
  (if
      (is-graph graph-id)

      (or
       (is-vertex
	graph-id
	vertex-id)
       (setf (gethash
	      (list 'vertex
		    graph-id
		    vertex-id)
	      *vertices*)
	     (list 'vertex
		   graph-id
		   vertex-id)))
    (error "Il grafo non esiste")))


(defun is-vertex (graph-id vertex-id)
  (if
      (is-graph graph-id)

      (or
       (gethash
	(list
	 'vertex
	 graph-id
	 vertex-id)
	*vertices*)
       nil)
    (error "Il grafo non esiste")))	


(defun graph-vertices (graph-id)
  (if
      (is-graph graph-id)

      (let ((vertex-list ()))
	(maphash
	 #'(lambda (k v)
	     (if
		 (equal
		  (second k)
		  graph-id)
		 
		 (push v vertex-list)))
	 *vertices*)
	vertex-list)
    
    (error "Il grafo non esiste")))


(defun new-arc (graph-id dep-id arr-id &optional (weight 1))
  (if
      (is-graph
       graph-id)
      
      (if
	  (is-vertex
	   graph-id
	   dep-id)
	  
	  (if
	      (is-vertex
	       graph-id
	       arr-id)

	      (if (is-arc
		   graph-id
		   dep-id
		   arr-id)

		  ;; se l'arco c'è già lo rimuovo da *arcs* e dalla lista dei
		  ;; vicini di dep e arr e lo reinserisco con il nuovo peso
		  (progn
		    (remhash
		     (list
		      'arc
		      graph-id
		      dep-id
		      arr-id)
		     *arcs*)
		    (remhash
		     (list
		      'arc
		      graph-id
		      arr-id
		      dep-id)
		     *arcs*)

		    ;; operazione applicata all'arco arr-id -> dep-id
		    (setf
		     (gethash
		      arr-id
		      *neighbors*)
		     (append
		      (remove
		       (find-arc
			graph-id
			arr-id
			dep-id
			(gethash
			 (list
			  graph-id
			  arr-id)
			 *neighbors*))
		       (gethash
			(list
			 graph-id
			 arr-id)
			*neighbors*))
		      (list (list
			     'arc
			     graph-id
			     arr-id
			     dep-id
			     weight))))
		    (setf
		     (gethash
		      (list
		       'arc
		       graph-id
		       arr-id
		       dep-id)
		      *arcs*)
		     (list
		      'arc
		      graph-id
		      arr-id
		      dep-id
		      weight))

		    ;; operazione applicata all'arco dep-id -> arr-id
		    (setf
		     (gethash
		      (list
		       graph-id
		       dep-id)
		      *neighbors*)
		     (append
		      (remove
		       (find-arc
			graph-id
			dep-id
			arr-id
			(gethash
			 dep-id
			 *neighbors*))
		       (gethash
			(list
			 graph-id
			 dep-id)
			*neighbors*))
		      (list (list
			     'arc
			     graph-id
			     dep-id
			     arr-id
			     weight))))
		    (setf
		     (gethash
		      (list
		       'arc
		       graph-id
		       dep-id
		       arr-id)
		      *arcs*)
		     (list
		      'arc
		      graph-id
		      dep-id
		      arr-id
		      weight)))

		;; se l'arco non c'è già lo aggiungo banalmente sia ad
		;; *arcs* che ai vicini di dep-id e arr-id
		(progn
		  (setf
		   (gethash
		    (list
		     'arc
		     graph-id
		     arr-id
		     dep-id)
		    *arcs*)
		   (list
		    'arc
		    graph-id
		    arr-id
		    dep-id
		    weight))
		  (setf
		   (gethash
		    (list
		     graph-id
		     arr-id)
		    *neighbors*)
		   (append
		    (gethash
		     (list
		      graph-id
		      arr-id)
		     *neighbors*)
		    (list (list
			   'arc
			   graph-id
			   arr-id
			   dep-id
			   weight))))
		  (setf
		   (gethash
		    (list
		     graph-id
		     dep-id)
		    *neighbors*)
		   (append
		    (gethash
		     (list
		      graph-id
		      dep-id)
		     *neighbors*)
		    (list (list
			   'arc
			   graph-id
			   dep-id
			   arr-id
			   weight))))
		  (setf
		   (gethash
		    (list
		     'arc
		     graph-id
		     dep-id
		     arr-id)
		    *arcs*)
		   (list
		    'arc
		    graph-id
		    dep-id
		    arr-id
		    weight))))
            (error "Il vertice di arrivo non esiste"))
	(error "Il vertice di partenza non esiste"))
    (error "Il grafo non esiste")))


(defun find-arc (graph-id dep-id arr-id neighbors)
  (if
      (is-graph graph-id)

      (if
	  (is-vertex graph-id dep-id)

	  (if
	      (is-vertex graph-id arr-id)
	      
	      (if
		  (not (null neighbors))
		  (if
		      (and
		       (equal
			(get-graph
			 (first neighbors))
			graph-id)
		       (equal
			(get-departure
			 (first neighbors))
			dep-id)
		       (equal
			(get-arrival
			 (first neighbors))
			arr-id))
		      (first neighbors)
		    (find-arc
		     graph-id
		     dep-id
		     arr-id
		     (rest neighbors)))
		nil)
	    (error "Il vertice destinazione non esiste"))
	(error "Il vertice partenza non esiste"))
    (error "Il grafo non esiste")))



(defun is-arc (graph-id dep-id arr-id)
  (if
      (is-graph
       graph-id)

      (if
	  (is-vertex
	   graph-id
	   dep-id)

	  (if
	      (is-vertex
	       graph-id
	       arr-id)

	      (or
	       (gethash
		(list
		 'arc
		 graph-id
		 dep-id
		 arr-id)
		*arcs*)
	       (gethash
		(list
		 'arc
		 graph-id
		 dep-id
		 arr-id)
		*arcs*)
	       nil)
	    (error "Il vertice di arrivo non esiste"))
	(error "Il vertice di partenza non esiste"))
    (error "Il grafo non esiste")))


(defun graph-arcs (graph-id)
  (if
      (is-graph
       graph-id)

      (let ((arc-list ()))
	(maphash
	 #'(lambda (k v)
	     (if
		 (equal
		  (second k)
		  graph-id)

		 (push v arc-list)))
	 *arcs*)
	arc-list)
    (error "Il grafo non esiste")))


(defun graph-vertex-neighbors (graph-id vertex-id)
  (if
      (is-graph
       graph-id)

      (if
	  (is-vertex
	   graph-id
	   vertex-id)

	  (let ((neighbors ()))
	    (maphash
	     #'(lambda (k v)
		 (if
		     (and
		      (equal
		       (second k)
		       graph-id)
		      (equal
		       (third k)
		       vertex-id))

		     (push v neighbors)))
	     *arcs*)
	    neighbors)
	(error "Il vertice non esiste"))
    (error "Il grafo non esiste")))


(defun graph-vertex-adjacent (graph-id vertex-id)
  (if
      (is-graph
       graph-id)

      (if
	  (is-vertex
	   graph-id
	   vertex-id)

	  (let ((adjacent ()))
	    (maphash
	     #'(lambda (k v)
		 (if
		     (and
		      (equal
		       (second k)
		       graph-id)
		      (equal
		       (third k)
		       vertex-id))

		     (push
		      (list
		       'vertex
		       graph-id
		       (fourth v))
		      adjacent)))
	     *arcs*)
	    adjacent)
	(error "Il vertice non esiste"))
    (error "Il grafo non esiste")))


(defun graph-print (graph-id)
  (if
      (is-graph
       graph-id)

      (progn
	(print graph-id)

	(format t "~%~%")
	
	(maphash
	 #'(lambda (k v)
	     (if
		 (equal
		  (second k)
		  graph-id)

		 (format t "~S~%" v)))
	 *vertices*)

	(format t "~%")
	
	(maphash
	 #'(lambda (k v)
	     (if
		 (equal
		  (second k)
		  graph-id)

		 (format t "~S~%" v)))
	 *arcs*))
    (error "Il grafo non esiste")))





;;; SEZIONE DI GESTIONE DELLE HEAP
(defun new-heap (heap-id &optional (capacity 42))
  (or
   (gethash heap-id *heaps*)
   (setf
    (gethash heap-id *heaps*)
    (list
     'heap
     heap-id
     0
     (make-array capacity)))))


(defun heap-size (heap-rep)
  (if
      (is-heap (second heap-rep))

      (third
       heap-rep)
    (error "La heap non esiste")))


(defun heap-id (heap-rep)
  (if
      (is-heap (second heap-rep))

      (second
       heap-rep)
    (error "La heap non esiste")))


(defun heap-actual-heap (heap-rep)
  (if
      (is-heap (second heap-rep))

      (fourth
       heap-rep)
    (error "La heap non esiste")))


(defun is-heap (heap-id)
  (if
      (gethash
       heap-id
       *heaps*)
      t
    nil))


(defun heap-delete (heap-id)
  (if (is-heap heap-id)

      (progn
	(remhash
	 heap-id
	 *heaps*)
	(maphash
	 #'(lambda (k v)
	     (declare
	      (ignore v))
	     (if
		 (equal
		  (first k)
		  heap-id)

		 (remhash
		  k
		  *positions*)))
	 *positions*)
	t)
    (error "La heap non esiste")))


(defun heap-empty (heap-id)
  (if (is-heap heap-id)

      (if
	  (equal
	   (heap-size
	    (gethash
	     heap-id
	     *heaps*))
	   0)

	  t
	nil)
    (error "La heap non esiste")))


(defun heap-not-empty (heap-id)
  (if (is-heap heap-id)

      (if
	  (not
	   (equal
	    (heap-size
	     (gethash
	      heap-id
	      *heaps*))
	    0))

	  t
	nil)
    (error "La heap non esiste")))

(defun heap-update (heap-id amount)
  (if
      (is-heap heap-id)

      (setf
       (gethash
	heap-id
	*heaps*)
       (list
	'heap
	heap-id
	(+
	 amount
	 (heap-size
	  (gethash
	   heap-id
	   *heaps*)))
	(heap-actual-heap
	 (gethash
	  heap-id
	  *heaps*))))
    (error "La heap non esiste")))

(defun get-father (heap-id pos)
  (aref
   (heap-actual-heap
    (gethash
     heap-id
     *heaps*))
   (fp pos)))

;; funzioni per l'accesso agli
;; elementi dello heap

(defun get-element (heap-id pos)
  (if
      (is-heap heap-id)

      (aref
       (heap-actual-heap
	(gethash
	 heap-id
	 *heaps*))
       pos)
    (error "La heap non esiste")))


(defun set-element (heap-id key value pos)
  (if
      (is-heap heap-id)

      (progn
	(setf
	 (aref
	  (heap-actual-heap
	   (gethash
	    heap-id
	    *heaps*))
	  pos)
	 (list
	  key
	  value))
	(setf
	 (gethash
	  (list
	   heap-id
	   value)
	  *positions*)
	 pos))
    (error "La heap non esiste")))


(defun swap-pos (heap-id key value pos)
  (if
      (is-heap heap-id)

      (progn
	(set-element
	 heap-id
	 (first
	  (get-element
	   heap-id
	   (fp pos)))
	 (second
	  (get-element
	   heap-id
	   (fp pos)))
	 pos)
	(set-element
	 heap-id
	 key
	 value
	 (fp pos)))
    (error "La heap non esiste")))


(defun swap (heap-id key value pos1 pos2)
  (if
      (is-heap heap-id)

      (progn
	(set-element
	 heap-id
	 (first
	  (get-element
	   heap-id
	   pos1))
	 (second
	  (get-element
	   heap-id
	   pos1))
	 pos2)
	(set-element
	 heap-id
	 key
	 value
	 pos1))
    (error "La heap non esiste")))


(defun delete-node (heap-id pos)
  (if
      (is-heap heap-id)

      (progn
	(remhash
	 (list
	  heap-id
	  (second
	   (aref
	    (heap-actual-heap
	     (gethash
	      heap-id
	      *heaps*))
	    pos)))
	 *positions*)
	(setf
	 (aref
	  (heap-actual-heap
	   (gethash
	    heap-id
	    *heaps*))
	  pos)
	 nil)
	t)
    (error "La heap non esiste")))


(defun get-position (heap-id key)
  (if      
      (gethash
       (list
	heap-id
	key)
       *positions*)

      (gethash
       (list
	heap-id
	key)
       *positions*)
    nil))


(defun fp (pos)
  (floor
   (1- pos) 2))


(defun left-son (pos)
  (1+
   (* 2 pos)))


(defun right-son (pos)
  (+ 2
     (* 2 pos)))


(defun heap-head (heap-id)
  (if
      (is-heap heap-id)

      (get-element
       heap-id
       0)
    (error "La heap non esiste")))


(defun heap-insert (heap-id key value)
  (if
      (is-heap heap-id)

      (if
	  (not
	   (=
	    (array-total-size
	     (heap-actual-heap
	      (gethash
	       heap-id
	       *heaps*)))
	    (heap-size
	     (gethash
	      heap-id
	      *heaps*))))

	  (if
	      (heap-empty
	       heap-id)

	      (progn
		(set-element
		 heap-id
		 key
		 value
		 0)
		(heap-update
		 heap-id
		 1)
		t)

	    (progn
	      (set-element
	       heap-id
	       key
	       value
	       (heap-size
		(gethash
		 heap-id
		 *heaps*)))
	      (heap-switch
	       heap-id
	       key
	       value
	       (heap-size
		(gethash
		 heap-id
		 *heaps*)))
	      (heap-update
	       heap-id
	       1)
	      t))

	;; se necessario allargo l'array che contiene lo heap
	(progn
	  (setf
	   (fourth
	    (gethash
	     heap-id
	     *heaps*))
	   (adjust-array
	    (heap-actual-heap
	     (gethash
	      heap-id
	      *heaps*))
	    (+
	     10
	     (array-total-size
	      (heap-actual-heap
	       (gethash
		heap-id
		*heaps*))))))
	  (heap-insert heap-id
		       key
		       value)))
    nil))


(defun heap-switch (heap-id key value pos)
  (if
      (is-heap heap-id)

      (if
	  (>
	   pos
	   0)

	  (progn
	    (if
		(>
		 (first
		  (get-element
		   heap-id
		   (fp pos)))
		 key)

		(progn
		  (swap-pos
		   heap-id
		   key
		   value
		   pos)
		  (heap-switch
		   heap-id
		   key
		   value
		   (fp pos)))
	      t)
	    t)
	t)
    (error "La heap non esiste")))		  


(defun heap-extract (heap-id)
  (if
      (is-heap heap-id)

      (if
	  (heap-not-empty
	   heap-id)

	  (let ((head
		 (heap-head
		  heap-id)))
	    
	    (if
		(=
		 (heap-size
		  (gethash
		   heap-id
		   *heaps*))
		 1)

		(progn
		  (delete-node
		   heap-id
		   (1- (heap-size
			(gethash
			 heap-id
			 *heaps*))))
		  (heap-update
		   heap-id
		   (- 1)))
	      
	      (progn
		(swap
		 heap-id
		 (first
		  head)
		 (second
		  head)
		 (1- (heap-size
		      (gethash
		       heap-id
		       *heaps*)))
		 0)
		(delete-node
		 heap-id
		 (1- (heap-size
		      (gethash
		       heap-id
		       *heaps*))))
		(heap-update
		 heap-id
		 (- 1))
		(heapify
		 heap-id
		 (first
		  (heap-head
		   heap-id))
		 (second
		  (heap-head
		   heap-id))
		 0)))
	    
	    ;; se possibile diminuisco la dimensione dello heap
	    (if
		(<
		 (heap-size
		  (gethash heap-id *heaps*))
		 (array-total-size
		  (heap-actual-heap
		   (gethash heap-id *heaps*))))

		(progn
		  (setf
		   (fourth
		    (gethash
		     heap-id
		     *heaps*))
		   (adjust-array
		    (heap-actual-heap
		     (gethash
		      heap-id
		      *heaps*))
		    (-
		     (array-total-size
		      (heap-actual-heap
		       (gethash
			heap-id
			*heaps*)))
		     (-
		      (array-total-size
		       (heap-actual-heap
			(gethash
			 heap-id
			 *heaps*)))
		      (heap-size
		       (gethash
			heap-id
			*heaps*))))))))
	    head)
	(error "La heap è vuota"))
    (error "La heap non esiste")))


(defun heapify (heap-id key value pos)
  (if
      (is-heap heap-id)

      (cond
       
       ;; Caso figlio unico
       ((and
	 (<
	  (left-son pos)
	  (heap-size
	   (gethash
	    heap-id
	    *heaps*)))
	 (>=
	  (right-son pos)
	  (heap-size
	   (gethash
	    heap-id
	    *heaps*))))

	;; Figlio minore padre
	(if
	    (<
	     (first
	      (get-element
	       heap-id
	       (left-son pos)))
	     key)
	    
	    (progn
	      (swap
	       heap-id
	       key
	       value
	       (left-son pos)
	       pos)
	      (heapify
	       heap-id
	       key
	       value
	       (left-son pos)))
	  t))

       ;; Caso dei due figli
       ((and
	 (<
	  (left-son pos)
	  (heap-size
	   (gethash
	    heap-id
	    *heaps*)))
	 (<
	  (right-son pos)
	  (heap-size
	   (gethash
	    heap-id
	    *heaps*))))

	;; Sinistro minore di destro
	(if
	    (<
	     (first
	      (get-element
	       heap-id
	       (left-son pos)))
	     (first
	      (get-element
	       heap-id
	       (right-son pos))))

	    ;; Sinistro minore di padre
	    (if
		(<
		 (first
		  (get-element
		   heap-id
		   (left-son pos)))
		 key)
		
	   	(progn
		  (swap
		   heap-id
		   key
		   value
		   (left-son pos)
		   pos)
		  (heapify
		   heap-id
		   key
		   value
		   (left-son pos)))

	      ;; Sinistro maggiore di padre
	      t)
	  
	  ;; Destro minore di padre
	  (if
	      (<
	       (first
		(get-element
		 heap-id
		 (right-son pos)))
	       key)

	      (progn
		(swap
		 heap-id
		 key
		 value
		 (right-son pos)
		 pos)
		(heapify
		 heap-id
		 key
		 value
		 (right-son pos)))

	    ;; Destro maggiore di padre
	    t)))
       
       (t
	t))
    (error "La heap non esiste")))


(defun heap-print (heap-id)
  (if
      (is-heap
       heap-id)

      (progn
	(format t "~S~%" heap-id)
	(format t "~S~%"
		(heap-size
		 (gethash
		  heap-id
		  *heaps*)))
	(format t "~S~%"
		(heap-actual-heap
		 (gethash
		  heap-id
		  *heaps*)))
	t)
    nil))


(defun positions-print (heap-id)
  (maphash
   #'(lambda (k v)
       (if
	   (equal
	    (first k)
	    heap-id)

	   (format t "~S~S~%" k v)))
   *positions*))





;;; ALGORITMO DI PRIM
(defun mst-prim (graph-id source)
  (if
      (is-graph graph-id)

      (if
	  (is-vertex graph-id source)

	  (progn
	    (clean-previous-mst graph-id)

	    ;; setto la sorgente
	    (setf
	     (gethash
	      (list
	       graph-id
	       source)
	      *vertex-keys*)
	     0)
	    (setf
	     (gethash
	      (list
	       graph-id
	       source)
	      *previous*)
	     nil)
	    (if
		(is-heap 'h)

		(progn
		  (heap-delete 'h)
		  (new-heap 'h
			    (hash-table-size *vertices*)))
	      (new-heap 'h
			(hash-table-size *vertices*)))

	    ;; inizializzo i nodi restanti e i relativi vertex-keys a inf
	    (mst-prim-initialization
	     graph-id
	     source
	     'h)
	    (heap-insertion-list
	     'h
	     (gethash
	      (list
	       graph-id
	       source)
	      *neighbors*))
	    (set-visited graph-id
			 source)
	    (set-vertex-key graph-id
			    (heap-head
			     'h))
	    (set-mst-previous graph-id
			      (heap-head
			       'h))
	    (set-visited graph-id
			 (second
			  (heap-head 'h)))
	    (mst-algorithm graph-id
			   (second
			    (heap-extract
			     'h))
			   'h)
	    nil)
	(error "Il vertice non esiste"))
    (error "Il grafo non esiste")))


;;; funzioni di appoggio mst-prim

(defun mst-algorithm (graph-id source heap-id)
  (if
      (heap-not-empty heap-id)
      (if
	  (not (=
		(gethash
		 (list graph-id source)
		 *vertex-keys*)
		MOST-POSITIVE-DOUBLE-FLOAT))

	  ;; Se il nodo è connesso visito i suoi vicini
	  (progn
	    (heap-insertion-list
	     heap-id
	     (gethash (list
		       graph-id
		       source)
		      *neighbors*))
	    (set-visited graph-id
			 (second
			  (heap-head heap-id)))
	    (mst-algorithm graph-id
			   (second
			    (heap-extract heap-id))
			   heap-id)
	    nil)

	;; altrimenti lo estraggo dalla heap e basta
	(progn
	  (set-vertex-key graph-id
			  (heap-head
			   heap-id))
	  (set-visited graph-id
		       (second
			(heap-head heap-id)))
	  (heap-extract heap-id)
	  (if (heap-not-empty heap-id)
	      (mst-algorithm graph-id
			     (second
			      (heap-head
			       heap-id))
			     heap-id))
	  nil))))


(defun clean-previous-mst (graph-id)
  (progn
    (maphash
     #'(lambda (k v)
	 (declare (ignore v))
	 (if (equal
	      (first k)
	      graph-id)
	     (remhash k *vertex-keys*)))
     *vertex-keys*)
    (maphash
     #'(lambda (k v)
	 (declare (ignore v))
	 (if (equal
	      (first k)
	      graph-id)
	     (remhash k *previous*)))
     *previous*)
    t))


(defun mst-prim-initialization (graph source heap)
  (if
      (is-graph graph)

      (if
	  (is-heap heap)

	  (maphash
	   #'(lambda (k v)
	       (declare (ignore v))
	       (if
		   (and
		    (equal
		     (second
		      k)
		     graph)
		    (not
		     (equal
		      (third k)
		      source)))

		   (progn

		     ;; Setto i vertex-key a infinito
		     (set-vertex-key
		      graph
		      (list
		       MOST-POSITIVE-DOUBLE-FLOAT
		       (third k)))
		     
		     ;; Metto tutti i nodi a non visitato
		     (set-not-visited
		      graph
		      (third k))

		     ;; Inserisco tutti i vertici nello heap con
		     ;; distanza infinito
		     (heap-insert
		      heap
		      MOST-POSITIVE-DOUBLE-FLOAT
		      (third k)))))
	   *vertices*)
	(error "La heap non esiste"))
    (error "Il grafo non esiste")))


(defun set-not-visited (graph-id vertex-id)
  (if (is-graph graph-id)

      (if (is-vertex graph-id
		     vertex-id)

	  (setf
	   (gethash
	    (list graph-id
		  vertex-id)
	    *visited*)
	   nil)
	(error "Il vertice non esiste"))
    (error "Il grafo non esiste")))


(defun set-visited (graph-id vertex-id)
  (if (is-graph graph-id)

      (if (is-vertex graph-id
		     vertex-id)

	  (setf
	   (gethash
	    (list graph-id
		  vertex-id)
	    *visited*)
	   t)
	(error "Il vertice non esiste"))
    (error "Il grafo non esiste")))


(defun heap-insertion-list (heap arc-list)
  (if
      (is-heap heap)

      (if
	  (not
	   (null arc-list))

	  (if
	      (not
	       (already-visited
		(get-graph
		 (first arc-list))
		(get-arrival
		 (first arc-list))))
	      (if
		  (<
		   (get-weight
		    (first arc-list))
		   (first
		    (get-element
		     heap
		     (get-position
		      heap
		      (get-arrival
		       (first arc-list))))))

		  ;; Se l'arco che sto inserendo pesa di meno di quello
		  ;; che già c'era lo sostituisco e cambio vertex-key e
		  ;; previous
		  (progn
		    (set-key
		     heap
		     (get-weight
		      (first arc-list))
		     (get-position
		      heap
		      (get-arrival
		       (first arc-list))))
		    (set-vertex-key
		     (get-graph
		      (first arc-list))
		     (get-element
		      heap
		      (get-position
		       heap
		       (get-arrival
			(first arc-list)))))
		    (setf
		     (gethash
		      (list
		       (get-graph
			(first arc-list))
		       (get-arrival
			(first arc-list)))
		      *previous*)
		     (get-departure
		      (first arc-list)))
		    (heap-switch
		     heap
		     (get-weight
		      (first arc-list))
		     (get-arrival
		      (first arc-list))
		     (get-position
		      heap
		      (get-arrival
		       (first arc-list))))
		    (heap-insertion-list
		     heap
		     (rest arc-list)))

		;; Altrimenti vado avanti ad inserire gli altri archi
		(heap-insertion-list
		 heap
		 (rest arc-list)))
	    (heap-insertion-list
	     heap
	     (rest arc-list)))
	t)
    (error "La heap non esiste")))


(defun set-key (heap weight pos)
  (setf
   (aref
    (heap-actual-heap
     (gethash
      heap
      *heaps*))
    pos)
   (list
    weight
    (second
     (aref
      (heap-actual-heap
       (gethash
	heap
	*heaps*))
      pos)))))


(defun already-visited (graph vertex)
  (if
      (is-graph graph)

      (if
	  (is-vertex graph
		     vertex)

	  (if
	      (gethash
	       (list
		graph
		vertex)
	       *visited*)
	      t
	    nil)
	(error "il vertice non esiste"))
    (error "Il grafo non esiste")))


(defun set-vertex-key (graph-id heap-entry)
  (setf
   (gethash
    (list
     graph-id
     (second
      heap-entry))
    *vertex-keys*)
   (first
    heap-entry)))


(defun set-mst-previous (graph-id heap-entry)
  (setf
   (gethash
    (list
     graph-id
     (second
      heap-entry))
    *previous*)
   (find-father
    graph-id
    heap-entry
    (gethash
     (list
      graph-id
      (second
       heap-entry))
     *neighbors*))))


(defun find-father (graph heap-entry arc-list)
  (if
      (is-graph
       graph)

      (if
	  (and
	   (equal
	    (first
	     heap-entry)
	    (get-weight
	     (first arc-list)))
	   (already-visited
	    graph
	    (get-arrival
	     (first arc-list))))

	  (get-arrival
	   (first arc-list))
	(find-father graph
		     heap-entry
		     (rest arc-list)))	   
    (error "Il grafo non esiste")))


;;; Funzioni di accesso ai campi degli archi
(defun get-graph (arc)
  (second arc))


(defun get-departure (arc)
  (third arc))


(defun get-arrival (arc)
  (fourth arc))


(defun get-weight (arc)
  (fifth arc))


(defun mst-vertex-key (graph-id vertex-id)
  (if
      (gethash
       (list
	graph-id
	vertex-id)
       *vertex-keys*)

      (gethash
       (list
	graph-id
	vertex-id)
       *vertex-keys*)))


(defun mst-previous (graph-id v)
  (if
      (gethash
       (list
	graph-id v)
       *previous*)

      (gethash
       (list
	graph-id v)
       *previous*)
    (error "Il padre non esiste")))


(defun mst-get (graph-id source)
  (if
      (is-graph graph-id)

      (if
	  (is-vertex graph-id source)

	  (if
	      (equal
	       (gethash
		(list graph-id source)
		*previous*)
	       nil)
	      
	      (mst-calculation
	       graph-id
	       (list-sort
		(regularize
		 (find-mst-previous
		  graph-id
		  source))))
	    (error "Il vertice non è sorgente di un mst"))
	(error "Il vertice non esiste"))
    (error "Il grafo non esiste")))


;;; Funzioni di appoggio mst-get

(defun mst-calculation (graph-id arc-list)
  (cond
   ((null arc-list) nil)
   (t
    (append
     (list (first
	    arc-list))
     (mst-calculation
      graph-id
      (list-sort
       (regularize
	(find-mst-previous
	 graph-id
	 (get-arrival
	  (first arc-list))))))
     (mst-calculation
      graph-id
      (rest arc-list))))))


(defun list-sort (arc-list)
  (sort arc-list
	#'(lambda (arc-a arc-b)
	    (if
		(=
		 (get-weight arc-a)
		 (get-weight arc-b))

		
		(if
		    (and
		     (numberp (get-arrival arc-a))
		     (numberp (get-arrival arc-b)))
		    (if
			(< (get-arrival arc-a)
			   (get-arrival arc-b))
			t
		      nil)
		  (if
		      (string<
		       (format nil "~A" (get-arrival arc-a))
		       (format nil "~A" (get-arrival arc-b)))

		      t
		    nil))

	      (if
		  (<
		   (get-weight arc-a)
		   (get-weight arc-b))
		  
		  t	  
		nil)))))


(defun regularize (arc-list)
  (mapcar
   #'(lambda (k)
       (gethash
	(list
	 'arc
	 (first k)
	 (second k)
	 (third k))
	*arcs*))
   arc-list))


(defun find-mst-previous (graph-id source)
  (let ((mst-previous-list ()))
    (maphash
     #'(lambda (k v)
	 (if
	     (and
	      (equal
	       (first k)
	       graph-id)
	      (equal
	       v
	       source))

	     (push
	      (list
	       graph-id
	       v
	       (second k))
	      mst-previous-list)))
     *previous*)
    mst-previous-list))





;;;; end-of-file -*- mst.lisp
