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

;; generator -> size -> (optional seed) -> value
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


;; generator combinators

(define (gen-pair genA genB)
  (define ((cons-curried a) b) (cons a b))
  (gen:app (gen:app (gen:pure cons-curried) genA) genB))

;; list generator -> generator list
(define (gen:sequence gens)
  (cond
   ((null? gens) (gen:pure (list)))
   (else         (gen-pair (car gens) (gen:sequence (cdr gens))))))

;; there is probably a better way of doing this
(define (gen:apply f gens)
  (gen:app (gen:pure (lambda (seq) (apply f seq))) (gen:sequence gens)))

(define (gen:one-of ls)
  (gen:map (lambda (i) (list-ref ls i))
           (arbitrary `(linear ,(length ls)))))

;; list (number, a) -> generator a
(define (gen:with-frequencies ls)
  (error "todo"))


;; CODE BELOW IS PROBABLY ALL WRONG
;; CODE BELOW IS PROBABLY ALL WRONG
;; CODE BELOW IS PROBABLY ALL WRONG
;; also move them to generator-instances.scm

(define (random-integer-in-range low high)
  (+ (random (- high low)) low))

(define arbitrary-uppercase-character
  (make-generator (lambda () (integer->char (+ (char->integer #\A)
                                               (random 26))))))
(add-generator 'uppercase-character arbitrary-uppercase-character)

(define arbitrary-lowercase-character
  (make-generator (lambda () (integer->char (+ (char->integer #\a)
                                               (random 26))))))
(add-generator 'lowercase-character arbitrary-lowercase-character)

(define arbitrary-letter
  (make-generator (lambda () (if (= (random 2) 0)
                                 ((generate arbitrary-lowercase-character))
                                 ((generate arbitrary-uppercase-character))))))
(add-generator 'letter arbitrary-letter)

(define arbitrary-numeric-character
  (make-generator (lambda () (integer->char (+ (char->integer #\0)
                                               (random 10))))))
(add-generator 'numeric-character arbitrary-numeric-character)

(define arbitrary-alphanumeric-character
  (make-generator (lambda () (if (> (random 62) 9)
                                 ((generate arbitrary-letter))
                                 ((generate arbitrary-numeric-character))))))
(add-generator 'alphanumeric-character arbitrary-alphanumeric-character)

(define arbitrary-character
  (make-generator (lambda () (integer->char (random char-code-limit)))))
(add-generator 'character arbitrary-character)

;; Generates a random string of a fixed length with characters of type character-type. Character-type must be specified, otherwise printing out characters might result in strings longer than intended.
(define (arbitrary-string length character-type)
  (if (<= length 1)
      (char->name (arbitrary-character character-type))
      (string-append (char->name (arbitrary-character character-type))
                 (arbitrary-string (- length 1) character-type))))

