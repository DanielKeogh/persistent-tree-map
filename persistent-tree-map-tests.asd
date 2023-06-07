;;;; persistent-tree-map-tests.asdf

(asdf:defsystem #:persistent-tree-map-tests
  :description "Tests for persistent-tree-map"
  :author "Daniel Keogh"
  :license "Eclipse 2.0"
  :depends-on (:persistent-tree-map :fiveam)
  :components ((:module "tests"
			:serial t
			:components ((:file "package")
				     (:file "main")))))
