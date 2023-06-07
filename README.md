# persistent-tree-map

A fast Persistent Tree Map implementation based upon Clojure's.

A Tree Map is a [Tree](https://en.wikipedia.org/wiki/Tree_(data_structure)) of key-value pairs.

The `persistent-tree-map` is an immutable red-black tree with O(log(n)) reads and writes.

## Usage

Given that `persistent-tree-map` collides with several important function definitions in the `:common-lisp` namespace it is recommended that this library is used with a local nickname. For example, like this:

```lisp
(defpackage my-package
    (:use #:cl)
    (:local-nicknames (#:ptm #:persistent-tree-map)))
```

### Comparer

Items in the tree-map are sorted by a comparer function taking two keys, returning less than zero when the first key comes before the second, 0 when they are equal and greater than zero when the first key is ordered after the second.

```lisp
(lambda (key1 key2) ...) ;; Expected return type of fixnum
```

For example, this is the implementation for the eq-comparer provided with this library:

```lisp
(defun eq-comparer (left right)
  (cond ((eq left right) 0)
	((null left) -1)
	((null right) 1)
	((< left right) -1)
	(:else 1)))
```

### API Description

**Constructor:**

```lisp
(ptm:make-tree-map nil 1 "a" 'foo)
;; #<treemap nil 1,"a" FOO>
```

**Adding:**

```lisp:
(ptm:add (ptm:make-tree-map 1 "foo") 1 "bar")
;; #<treemap 1 "bar">
```

**Removing:**
```lisp
(ptm:remove (ptm:make-tree-map 1 1 2 2) 1)
;; #<treemap 2 2>
```

**Finding values:**
```lisp
(ptm:value (ptm:make-tree-map "foo" 2 "bar") 1)
;; "foo"
```

**Testing keys:**

```lisp
(ptm:has-key (ptm:make-tree-map 1 1 2 2) 1)
;; T
(ptm:has-key (ptm:make-tree-map 1 1 2 2) 100)
;; nil
```

**Length/Count:**

```lisp
(ptm:length (ptm:make-tree-map 1 "foo" 2 "bar"))
;; 2
```

**Mapping:**

```lisp
(ptm:map (ptm:make-tree-map 1 100 2 200 3 300)
         (lambda (key val) (+ key val))
;; (101 202 303)
```

**Reduce:**

```lisp
(ptm:reduce (ptm:make-tree-map 1 0 2 0 3 0)
            (lambda (start key val) (+ start key val))
            0)
;; 6
```
