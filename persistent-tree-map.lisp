;;;; persistent-tree-map.lisp

(in-package #:persistent-tree-map)

(proclaim '(optimize (speed 3) (safety 0) (debug 0)))
;; struct definitions

(defstruct node
  (key nil :read-only t)
  (value nil :read-only t)
  (left nil :type (or null node) :read-only t)
  (right nil :type (or null node) :read-only t)
  (is-red nil :type boolean :read-only t))

(defstruct (persistent-tree-map (:conc-name ptm-))
  (count nil :type fixnum :read-only t)
  (comparator nil :type (or (function (t t) fixnum) symbol) :read-only t)
  (tree nil :type (or node null)))

;; accessor macros

(defmacro with-ptm ((tree comparator count) ptm &body body)
  `(with-accessors ((,tree ptm-tree)
		    (,comparator ptm-comparator)
		    (,count ptm-count))
       ,ptm
     ,@body))

(defmacro with-node ((key val left right) node &body body)
  `(with-accessors ((,key node-key)
		    (,val node-value)
		    (,left node-left)
		    (,right node-right))
       ,node
     ,@body))

;; impl

(defun node-redp (node)
  (declare (optimize (speed 3) (safety 0))
	   (type (or node null) node))
  (and node (node-is-red node)))

(defun node-blackp (node)
  (declare (optimize (speed 3) (safety 0))
	   (type (or node null) node))
  (and node (not (node-is-red node))))

(defun red (key val left right)
  (declare (optimize (speed 3) (safety 0)))
  (make-node :key key :value val :left left :right right :is-red t))

(defun black (key val left right)
  (make-node :key key :value val :left left :right right :is-red nil))

(defun blacken (node)
  (if (node-is-red node)
      (with-node (key val left right) node
	(make-node :key key :value val :left left :right right :is-red nil))
      node))

(defun redden (node)
  (if (node-is-red node)
      node
      (with-node (key val left right) node
	(make-node :key key :value val :left left :right right :is-red nil))))

(defun left-balance (key val ins right)
  (cond ((and (node-is-red ins) (node-is-red (node-left ins)))

	 (red (node-key ins) (node-value ins)
		   (blacken (node-left ins))
		   (black key val (node-right ins) right)))
	((and (node-is-red ins) (node-is-red (node-right ins)))
	 (let ((ins-right (node-right ins)))
	   (red (node-key ins-right) (node-value ins-right)
		     (black (node-key ins) (node-value ins)
				 (node-left ins) (node-left (node-right ins)))
		     (black key val (node-right (node-right ins)) right))))
	(t (black key val ins right))))

(defun right-balance (key val left ins)
  (cond ((and (node-is-red ins) (node-is-red (node-right ins)))
	 (red (node-key ins) (node-value ins)
		   (black key val left (node-left ins))
		   (blacken (node-right ins))))
	((and (node-is-red ins) (node-is-red (node-left ins)))
	 (let ((ins-left (node-left ins)))
	   (red (node-key ins-left) (node-value ins-left)
		     (black key val left (node-left ins-left))
		     (black (node-key ins) (node-value ins)
				 (node-right ins-left)
				 (node-right ins)))))
	(t (black key val left ins))))

(defun balance-left-del (key val del right)
  (cond ((node-redp del)
	 (red key val (blacken del) right))
	((node-blackp right)
	 (right-balance key val del (redden right)))
	((and (node-redp right) (node-blackp (node-left right)))
	 (let ((rleft (node-left right)))
	   (red (node-key rleft) (node-value rleft)
		(black key val del (node-left rleft))
		(right-balance (node-key right) (node-value right)
			       (node-right (node-left right))
			       (redden (node-right right))))))
	(t (error "Invariant violation"))))

(defun balance-right-del (key val left del)
  (cond ((node-redp del)
	 (red key val left (blacken del)))
	((node-blackp left)
	 (left-balance key val (redden left) del))
	((and (node-redp left) (node-blackp (node-right left)))
	 (let ((lright (node-right left)))
	   (red (node-key lright) (node-value lright)
		     (left-balance (node-key left) (node-value left)
				   (redden (node-left left))
				   (node-left lright))
		     (black key val (node-right lright) del))))
	(t (error "Invariant violation"))))

(defun node-append (left right)
  (cond ((not left) right)
	((not right) left)
	((node-redp left)
	 (if (node-redp right)
	     (let ((app (append (node-right left) (node-left right))))
	       (if (node-redp app)
		   (red (node-key app)
			(node-value app)
			(red (node-key left) (node-value left)
			     (node-left left) (node-left app))
			(red (node-key right) (node-value right)
			     (node-right app) (node-right right)))
		   (red (node-key left) (node-value left)
			(node-left left)
			(red (node-key right) (node-value right)
			     app (node-right right)))))
	     ;; else
	     (red (node-key left) (node-value left)
		  (node-left left) (node-append (node-right left) (node-right right)))))
	
	((node-redp right)
	 (red (node-key right) (node-value right)
	      (node-append left (node-left right)) (node-right right)))
	(t (let ((app (node-append (node-right left) (node-left right))))
	     (if (node-redp app)
		 (red (node-key app)
		      (node-value app)
		      (black (node-key left) (node-value left)
			     (node-left left) (node-left app))
		      (black (node-key right) (node-value right)
			     (node-right app) (node-right right)))

		 (balance-left-del (node-key left) (node-value left)
				   (node-left left)
				   (black (node-key right)
					  (node-value right)
					  app
					  (node-right right))))))))

(defun do-compare (ptm key1 key2)
  (let ((comp (ptm-comparator ptm)))
    (funcall comp key1 key2)))

(defun node-remove (ptm n key)
  (when n
    (let ((c (do-compare ptm key (node-key n))))
      (if (= c 0)
	  (values (node-append (node-left n)
			       (node-right n))
		  n)
	  (multiple-value-bind (del found)
	      (if (< c 0)
		  (node-remove ptm (node-left n) key)
		  (node-remove ptm (node-right n) key))
	    (when (or del found)
	      (values
	       (if (< c 0)
		   (if (node-blackp (node-left n))
		       (balance-left-del (node-key n) (node-value n) del (node-right n))
		       (red (node-key n) (node-value n) del (node-right n)))
		   (if (node-blackp (node-right n))
		       (balance-right-del (node-key n) (node-value n) (node-left n) del)
		       (red (node-key n) (node-value n) (node-left n) del)))
	       found)))))))

(defun node-balance-left (node parent)
  (with-node (key val left right) parent
    (black key val node right)))

(defun node-balance-right (node parent)
  (with-node (key val left right) parent
    (black key val left node)))

(defun node-add-left (node ins)
  (if (node-is-red node)
      (with-node (key val left right)
		 node
	(red key val ins right)))
      (node-balance-left ins node))

(defun node-add-right (node ins)
  (if (node-is-red node)
      (with-node (key val left right)
		 node
	(red key val left ins))
      (node-balance-right ins node)))

(defun ptm-add (ptm node key val)
  (if node
      (let ((c (do-compare ptm key (node-key node))))
	(if (= c 0)
	    (values nil node)
	    (multiple-value-bind (ins found)
		(if (< c 0)
		    (ptm-add ptm (node-left node) key val)
		    (ptm-add ptm (node-right node) key val))
	      (values 
	       (if ins
		   (if (< c 0)
		       (node-add-left node ins)
		       (node-add-right node ins))
		   ins)
	       found))))
      ;; else
      (values (make-node :key key :value val :is-red t) nil)))

(defun node-replace (node key val left right)
  (if (node-is-red node)
      (red key val left right)
      (black key val left right)))

(defun ptm-replace (ptm node key val)
  (with-node (n-key n-val left right) node
    (let ((c (do-compare ptm key n-key)))
      (node-replace node n-key 
		    (if (= c 0) val n-val)
		    (if (< c 0) (ptm-replace ptm left key val) left)
		    (if (> c 0) (ptm-replace ptm right key val) right)))))

(defun ptm-assoc (ptm key val)
  (with-ptm (tree comp cnt) ptm
    (multiple-value-bind (node found)
	(ptm-add ptm tree key val)
      (if node
	  (make-persistent-tree-map :comparator comp
				    :count (1+ cnt)
				    :tree (blacken node))
	  (if (and found (eq val (node-value found)))
	      ptm
	      (make-persistent-tree-map :comparator comp
					:count cnt
					:tree (ptm-replace ptm tree key val)))))))

(defun ptm-without (ptm key)
  (with-ptm (tree comp cnt) ptm
    (multiple-value-bind (node found)
	(node-remove ptm tree key)
      (if found 
	  (if (> cnt 1)
	      (make-persistent-tree-map :comparator comp
					:count (1- cnt)
					:tree (blacken node))
	      (make-persistent-tree-map :comparator comp
					:count 0
					:tree nil))
	  ptm))))

(defun ptm-lookup (ptm key)
  (loop for node = (ptm-tree ptm)
	  then (if (< comparison 0)
		   (node-left node)
		   (node-right node))
	while node
	for comparison = (do-compare ptm key (node-key node))
	when (= comparison 0)
	  do (return (values t (node-value node)))))

(defun create (comp &rest items)
  (loop for ret = (make-persistent-tree-map :count 0 :comparator comp)
	  then (ptm-assoc ret key val)
	for lst on items by #'cddr
	for (key val) = lst
	do
	   (when (cdr lst)
	     (error "No value supplied for key: ~a" key))
	finally (return ret)))

(declaim (ftype (function (t t) fixnum) eq-comparer))
(defun eq-comparer (left right)
  (cond ((eq left right) 0)
	((null left) -1)
	((null right) 1)
	((< left right) -1)
	(:else 1)))

(defun make-iterator (ptm)
  (let ((current-node (list (ptm-tree ptm))))
    (labels ((left-most ()
	       (loop for top = (car current-node)
		     for left = (node-left top)
		     while left
		     do (push left current-node))))
      (left-most)
      (lambda ()
	(let ((n (pop current-node)))
	  (if n
	      (progn
		(when (node-right n)
		  (push (node-right n) current-node)
		  (left-most))
		(values t (node-key n) (node-value n)))
	      (values nil nil nil)))))))

(defmethod print-object ((ptm persistent-tree-map) stream)
  (print-unreadable-object (ptm stream :type 'persistent-tree-map)
    (labels ((print-node (node)
	       (with-node (key val left right)
			  node
		 (when left
		   (print-node left)
		   (write-char #\  stream))
		 (prin1 key stream)
		 (write-char #\  stream)
		 (prin1 val stream)
		 (when right
		   (write-char #\  stream)
		   (print-node right))
		 )))
      (write-char #\( stream)
      (when (> (ptm-count ptm) 0) (print-node (ptm-tree ptm)))
      (write-char #\) stream))))
