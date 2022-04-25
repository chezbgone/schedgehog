;; A generator represents a source of values for randomly checking a property.
;;
;; Intuitively, it can be thought of as a structure containing two components:
;; - A generating function `size -> seed -> value`
;; - A shrinking function `value -> list value`
;; One desired property is to be able to map a function over a generator object,
;; which requires covariance. To make this structure covariant,
;; we can recursively apply the shrinking function to the value obtained from
;; he generating function. This constructs a tree of ways to shrink the data
;; (which we lazily generate to save memory).
;;
;; As a result, a generator is a function of the form
;; gen : size -> seed -> lazy-tree
(define-record-type generator
  (%make-generator gen)
  generator?
  (gen get-gen))

;; generator -> size -> (optional seed) -> value
(define (generate generator size #!optional seed)
  (cond
   ((default-object? seed) ((get-gen generator) size (make-random-state #t)))
   (else                   ((get-gen generator) size seed))))

(define generator-store (make-hash-table))
(define (set-generator! type gen)
  (hash-table-set! generator-store type gen))

;; symbol -> generator (if found)
(define (arbitrary type)
  (hash-table-ref generator-store type
                  (lambda () (error "no generator found for" type))))

(define ((make-shrink-tree shrink-fn) value)
  (make-lazy-tree
   value
   (map (make-shrink-tree shrink-fn)
        (shrink-fn value))))

;; shrink-function -> value -> lazy-tree
(define (shrinkable-via shrink-fn value)
  ((make-shrink-tree shrink-fn) value))

;; value -> lazy-tree
(define (unshrinkable value)
  (make-lazy-tree value (list)))


;;; generator combinators

;; monadic return
;; value -> generator
(define (constant-gen val)
  (%make-generator
   (lambda (size seed)
     (unshrinkable val))))


;; CODE BELOW IS PROBABLY ALL WRONG
;; CODE BELOW IS PROBABLY ALL WRONG
;; CODE BELOW IS PROBABLY ALL WRONG
;; also move them to generator-instances.scm

(define arbitrary-boolean
  (make-generator (lambda () (= 0 (random 2)))))
(add-generator 'boolean arbitrary-boolean)

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

