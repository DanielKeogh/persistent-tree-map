;;;; api.lisp

(in-package #:persistent-tree-map)

(defun make-tree-map (comparer-fn &rest pairs)
  "Make a persistent-tree-map. Comparer should be a (function (x y) fixnum) that returns 0 for equal items, 1 when (< x y) and -1 when (> y x)."
  (loop with map = (make-persistent-tree-map :count 0 :comparator comparer-fn :tree nil)
	for pair on pairs by #'cddr
	for (key val) = pair
	do (unless (cdr pair) (error "Uneven number of pairs"))
	   (setf map (ptm-assoc map key val))
	finally (return map)))

(defun add (tree-map key val)
  "Create a new map with the given key-value pair"
  (ptm-assoc tree-map key val))

(defun remove (tree-map key)
  "Remove the pair from the tree-map"
  (ptm-without tree-map key))

(defun value (tree-map key &optional not-found)
  "Get the value for a given key"
  (multiple-value-bind (found result)
      (ptm-lookup tree-map key)
    (values
     (if found
	 result
	 not-found)
     found)))

(defun has-key (tree-map key)
  "Test if the key is in the tree map"
  (ptm-lookup tree-map key))

(defun map (tree-map fn)
  "Apply a (lambda (x y)) to each key-value pair and collect the results into a list"
  (loop with itr = (make-iterator tree-map)
	for (continue key value) = (multiple-value-list (funcall itr))
	while continue
	collect (funcall fn key value)))

(defun reduce (tree-map fn &optional start-val)
  "Apply (lambda (start key val)) to aggregate all pairs of a persistent tree map"
  (loop with itr = (make-iterator tree-map)
	for (continue key value) = (multiple-value-list (funcall itr))
	while continue
	for result = (funcall fn start-val key value)
	  then (funcall fn result key value)
	finally (return result)))

(defun length (tree-map)
  "The number of pairs in the tree map"
  (ptm-count tree-map))
