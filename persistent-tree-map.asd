;;;; persistent-tree-map.asd

(asdf:defsystem #:persistent-tree-map
  :description "Persistent/Immutable red-black tree based upon the implementation in Clojure"
  :author "Daniel Keogh"
  :license  "Eclipse 2.0"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "persistent-tree-map")
	       (:file "api")))
