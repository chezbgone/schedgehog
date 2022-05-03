#|
A generator represents a source of values for randomly checking a property.

Intuitively, it can be thought of as a structure containing two components:
- A generating function `size -> seed -> value`
- A shrinking function `value -> list value`
One desired property is to be able to map a function over a generator object,
which requires covariance. To make this structure covariant,
we can recursively apply the shrinking function to the value obtained from
the generating function. This constructs a tree of ways to shrink the data
(which we lazily generate to conserve memory).

As a result, a generator is a function of the form
generator a : {
  gen : size -> seed -> lazy-tree a
}
|#
(define-record-type generator
  (%make-generator gen)
  generator?
  (gen get-gen))

;; generator -> (optional size) -> (optional seed) -> lazy-tree
(define default-size 20)
(define (generate generator #!optional size seed)
  (let ((sz (if (default-object? size) default-size size))
        (sd (if (default-object? seed) (make-random-state #t) seed)))
    ((get-gen generator) sz sd)))

;; generate-an -> (optional size) -> (optional seed) -> value
(define (generate-an generator #!optional size seed)
  (lazy-tree-value (generate generator size seed)))

;; for the sake of English idiosyncrasies
(define generate-a generate-an)

;;; constructing shrink trees

;; shrink-function -> value -> lazy-tree
(define (shrinkable-via shrink-fn value)
  ;; (value -> [value]) -> value -> lazy-tree value
  (define ((make-shrink-tree shrink-fn) value)
    (make-lazy-tree
     value
     (map (lambda (shrunk-value)
            (lambda () ((make-shrink-tree shrink-fn) shrunk-value)))
          (shrink-fn value))))
  ((make-shrink-tree shrink-fn) value))

;; value -> lazy-tree
(define (unshrinkable value)
  (lazy-tree-node value))


;;; creating generators

;; (size -> seed -> value) -> (value -> [value]) -> generator
;; (     gen function    ) -> (shrink function ) -> generator
(define (make-generator gen shrink-fn)
  (%make-generator
   (lambda (size seed)
     (shrinkable-via shrink-fn (gen size seed)))))

;; (size -> seed -> value) -> generator
;; (     gen function    ) -> generator
(define (make-generator-without-shrinking gen)
  (%make-generator
   (lambda (size seed)
     (unshrinkable (gen size seed)))))

;; generator -> generator
(define (remove-shrinking generator)
  (%make-generator
   (lambda (size seed)
     (unshrinkable (lazy-tree-value (generate generator size seed))))))

;; shrink-function -> generator -> generator
(define (replace-shrinking shrink-fn generator)
  (%make-generator
   (lambda (size seed)
     (shrinkable-via shrink-fn
                     (lazy-tree-value (generate generator size seed))))))


;;; `arbitrary` generic function

(define generator-store (make-hash-table))
(define (set-generator! type gen)
  (hash-table-set! generator-store type gen))

;; symbol -> generator (if found)
(define (arbitrary type)
  (cond
   ((symbol? type)
    (hash-table-ref generator-store type
                    (lambda () (error "no generator found for" type))))
   ((list? type)
    (apply (arbitrary (car type))
           (cdr type)))
   (else (error "arbitrary instance not found for" type))))


;;; generator utilities

;; introduce size parameter
;; (size -> generator a) -> (generator a)
(define (sized s-gen)
  (%make-generator
   (lambda (size seed)
     (generate (s-gen size) size seed))))

;; fixed size
;; size -> (generator a) -> (generator a)
(define (resize size generator)
  (%make-generator
   (lambda (old-size seed)
     (generate generator size seed))))

;; (a -> b) -> (generator a -> generator b)
(define (gen:map f generator)
  (%make-generator
   (lambda (size seed)
     (lazy-tree:map f (generate generator size seed)))))

;; applicative pure
;; value -> generator
(define (gen:pure val)
  (%make-generator
   (lambda (size seed)
     (unshrinkable val))))
(define constant-gen gen:pure)

;; generator (a -> b) -> generator a -> generator b
(define (gen:app gen-f gen-a)
  (%make-generator
   (lambda (size seed)
     (let ((tree-f (generate gen-f size seed))
           (tree-a (generate gen-a size seed)))
       (lazy-tree:interleave tree-f tree-a)))))

;; (generator a) -> (a -> generator b) -> (generator b)
(define (gen:bind gen continuation)
  (%make-generator
   (lambda (size seed)
     (let* ((tree-a (generate gen size seed))          ; lazy-tree a
            (tree-cont                                 ; a -> lazy-tree b
             (lambda (a)
               (generate (continuation a) size seed))))
       (lazy-tree:bind tree-a tree-cont)))))

(define gen:then gen:bind)
