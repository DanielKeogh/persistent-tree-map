;;;; tests/main.lisp

(in-package #:persistent-tree-map-tests)

(def-suite all-tests
  :description "Main test suite for persistent-tree-map")

(in-suite all-tests)

(test make-tree-map
  (is (ptm:persistent-tree-map-p (ptm:make-tree-map 'ptm:eq-comparer)))
  (is (ptm:persistent-tree-map-p (ptm:make-tree-map 'ptm:eq-comparer 1 1)))
  (is (ptm:persistent-tree-map-p (ptm:make-tree-map 'ptm:eq-comparer "one" "one")))
  (is (ptm:persistent-tree-map-p (ptm:make-tree-map 'ptm:eq-comparer nil "one"))))

(test value
  (is (= 1 (ptm:value (ptm:make-tree-map 'ptm:eq-comparer nil 1) nil)))
  (is (= 1 (ptm:value (ptm:make-tree-map 'ptm:eq-comparer 1 1) 1)))
  (is (= 1 (ptm:value (ptm:make-tree-map 'ptm:eq-comparer "boo" 1) "boo")))
  (is (null (ptm:value (ptm:make-tree-map 'ptm:eq-comparer "noo" 1) "boo"))))

(test add
  (is (= 1 (ptm:length (ptm:add (ptm:make-tree-map 'ptm:eq-comparer) 1 1))))
  (is (= 2 (ptm:length (ptm:add (ptm:make-tree-map 'ptm:eq-comparer 1 2) 2 2))))
  (is (= 1 (ptm:length (ptm:add (ptm:make-tree-map 'ptm:eq-comparer 1 1) 1 2))))
  (is (= 1 (ptm:length (ptm:add (ptm:make-tree-map 'ptm:eq-comparer 1 1) 1 2))))
  (is (= 1 (ptm:length (ptm:add (ptm:make-tree-map 'ptm:eq-comparer) nil 2))))
  (is (= 1000 (ptm:length (loop with map = (ptm:make-tree-map 'ptm:eq-comparer)
				    for i from 1 to 1000
				    do (setf map (ptm:add map i i))
				    finally (return map))))))

(test remove
  (is (= 0 (ptm:length (ptm:remove (ptm:make-tree-map 'ptm:eq-comparer nil 1) nil))))
  (is (= 0 (ptm:length (ptm:remove (ptm:make-tree-map 'ptm:eq-comparer 1 1) 1))))
  (is (= 1 (ptm:length (ptm:remove (ptm:make-tree-map 'ptm:eq-comparer nil 1) 1)))))

(test has-key
  (let ((map (ptm:make-tree-map 'ptm:eq-comparer)))
    (dotimes (i 1000)
      (setf map (ptm:add map i i)))
    (is (loop for i from 0 below 1000
	      always (ptm:has-key map i)))))

(test has-key-not
  (let ((map (ptm:make-tree-map 'ptm:eq-comparer)))
    (dotimes (i 1000)
      (setf map (ptm:add map i i)))
    (is (loop for i from 0 below 1000
	      always (= i (ptm:value map i))))))

(test value
  (let ((map (ptm:make-tree-map 'ptm:eq-comparer)))
    (dotimes (i 1000)
      (setf map (ptm:add map i i)))
    (is (loop for i from 0 below 1000
	      always (= i (ptm:value map i))
	      do (setf map (ptm:remove map i))
	      never (ptm:value map i)))))

(test persistent
  (let* ((map1 (ptm:make-tree-map 'ptm:eq-comparer 1 1 2 2 3 3))
	 (map2 (ptm:remove map1 3))
	 (map3 (ptm:add map1 4 4)))

    (is (ptm:has-key map1 1))
    (is (ptm:has-key map2 1))
    (is (ptm:has-key map3 1))

    (is (ptm:has-key map1 2))
    (is (ptm:has-key map2 2))
    (is (ptm:has-key map3 2))

    (is (ptm:has-key map1 3))
    (is (null (ptm:has-key map2 3)))
    (is (ptm:has-key map3 3))

    (is (null (ptm:has-key map1 4)))
    (is (null (ptm:has-key map2 4)))
    (is (ptm:has-key map3 4))))

(test add-remove-1000
  (let ((map (ptm:make-tree-map 'ptm:eq-comparer)))
    (dotimes (i 1000)
      (setf map (ptm:add map i i)))
    (is (= 1000 (ptm:length map)))
    (dotimes (i 1000)
      (setf map (ptm:remove map i)))
    (is (= 0 (ptm:length map)))))

(test map
  (let ((map (loop for i below 10
		   for m = (ptm:make-tree-map 'ptm:eq-comparer 0 0)
		   then (ptm:add m i i)
		   finally (return m))))
    
    (is (= (loop for i from 0 below 10 sum i)
	   (reduce (lambda (x y) (+ x y))
		   (ptm:map map (lambda (key val) (declare (ignore key)) val)))))

    (is (= (loop for i from 0 below 10 sum i)
	 (reduce (lambda (x y) (+ x y))
		 (ptm:map map (lambda (key val) (declare (ignore val)) key)))))))


(test reduce
  (let ((map (ptm:make-tree-map 'ptm:eq-comparer)))
    (dotimes (i 1000)
      (setf map (ptm:add map i i)))

    (is (= (loop for i below 1000 sum i)
	   (ptm:reduce map (lambda (aggr key val) (+ aggr key))
		       0)))))
