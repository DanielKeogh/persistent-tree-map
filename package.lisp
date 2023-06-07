;;;; package.lisp

(defpackage #:persistent-tree-map
  (:documentation "Persistent Tree Map datastructure based upon Clojure")
  (:use #:cl)
  (:shadow
   #:length
   #:map
   #:reduce
   #:remove)
  (:export
   ;;; api.lisp
   ;; Builders
   #:make-tree-map
   #:add
   #:remove
   #:value
   #:has-key
   #:map
   #:reduce
   #:length

   ;; Comparers
   #:eq-comparer

   ;; Types
   #:tree-map
   #:tree-map-p
   #:persistent-tree-map
   #:persistent-tree-map-p))
