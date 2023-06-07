;;;; tests/package.lisp

(defpackage #:persistent-tree-map-tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:ptm #:persistent-tree-map))
  (:export #:run!
	   #:all-tests))
