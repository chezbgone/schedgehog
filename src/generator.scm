;; A generator represents a source of values for randomly checking a property.
;;
;; Intuitively, it can be thought of as a structure containing two components:
;; - A generating function `size -> seed -> value`
;; - A shrinking function `value -> list value`
;; One desired property is to be able to map a function over a generator object,
;; which requires covariance. To make this structure covariant,
;; we can recursively apply the shrinking function to the value obtained from
;; the generating function. This constructs a tree of ways to shrink the data
;; (which we lazily generate to conserve memory).
;;
;; As a result, a generator is a function of the form
;; generator a : {
;;   gen : size -> seed -> lazy-tree a
;; }
(define-record-type generator
  (%make-generator gen)
  generator?
  (gen get-gen))

;; generator -> (optional size) -> (optional seed) -> value
(define default-size 20)
(define (generate generator #!optional size seed)
  (let ((sz (if (default-object? size) default-size size))
        (sd (if (default-object? seed) (make-random-state #t) seed)))
    ((get-gen generator) sz sd)))

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
    (case (car type)
      ((linear range range-from)
       (apply (hash-table-ref generator-store (car type))
              (cdr type)))
      ((list) (display "generator TODO"))
      (else (begin (display type)
                   (display " not found")))
      ))))

;; (val -> [val]) -> val -> lazy-tree val
(define ((make-shrink-tree shrink-fn) value)
  (make-lazy-tree
   value
   (map (lambda (shrunk-value)
          (lambda () ((make-shrink-tree shrink-fn) shrunk-value)))
        (shrink-fn value))))

;; shrink-function -> value -> lazy-tree
(define (shrinkable-via shrink-fn value)
  ((make-shrink-tree shrink-fn) value))

;; value -> lazy-tree
(define (unshrinkable value)
  (lazy-tree-node value))


;;; generator utilities

;; shrink-function -> generator -> generator
(define ((replace-shrink-tree shrink-fn) generator)
  (%make-generator
   (lambda (size seed)
     (shrinkable-via shrink-fn
                     (lazy-tree-value (generate generator size seed))))))

;; (size -> generator a) -> (generator a)
(define (sized s-gen)
  (%make-generator
   (lambda (size seed)
     (generate (s-gen size) size seed))))

;; sizing
(define (resize size generator)
  (%make-generator
   (lambda (old-size seed)
     (generate generator size seed))))

;; generating-function -> generator
(define (no-shrink generator)
  (%make-generator
   (lambda (size seed)
     (unshrinkable (lazy-tree-value (generate generator size seed))))))

;; (a -> b) -> (generator a -> generator b)
(define (gen:map f generator)
  (%make-generator
   (lambda (size seed)
     (lazy-tree:map f (generate generator size seed)))))

;; applicative pure
;; value -> generator
(define (constant-gen val)
  (%make-generator
   (lambda (size seed)
     (unshrinkable val))))

(define gen:pure constant-gen)

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

;;; generator combinators
;; generator -> generator -> generator
(define (gen-pair genA genB)
  (define ((cons-curried a) b) (cons a b))
  (gen:app (gen:map cons-curried genA) genB))

;; list generator -> generator list
(define (gen:list . gens)
  (cond
   ((null? gens) (gen:pure (list)))
   (else         (gen-pair (car gens) (gen:sequence (cdr gens))))))

;; list generator -> generator list
(define (gen:sequence gens)
  (apply gen:list gens))

;; there is probably a better way of doing this
(define (gen:apply f . gens)
  (gen:map (lambda (seq) (apply f seq)) (gen:sequence gens)))

(define (gen:one-of ls)
  (gen:map (lambda (i) (list-ref ls i))
           (arbitrary `(linear ,(length ls)))))

(define (gen:one-of_ ls)
  (no-shrink (gen:one-of ls)))

;; list generators -> generator
(define (gen:select-from ls)
  (gen:then (arbitrary `(linear ,(length ls)))
            (lambda (idx)
              (list-ref ls idx))))

;; list (number, a) -> generator a
(define (gen:with-frequencies ls)
  (define (create-lst-with-freqs freqs elements)
    (if (> (length freqs) 0)
        (if (= (car freqs) 0)
            (create-lst-with-freqs (cdr freqs) (cdr elements))
            (cons (car elements) (create-lst-with-freqs (cons (- (car freqs) 1)
                                                              (cdr freqs))
                                                        elements)))
        '()))
  (let* ((freq-lst (map car ls))
         (gen-lst (map cdr ls))
         (list-elements-weighted (create-lst-with-freqs freq-lst gen-lst)))
    (gen:select-from list-elements-weighted)))
