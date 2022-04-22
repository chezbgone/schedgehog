;; A generator is a function that returns
;; an arbitrary element of the type it represents.
(define-record-type generator
  (make-generator generate)
  generator?
  (generate generate))

(define generator-store (make-hash-table))
(define (add-generator type gen)
  (hash-table-set! generator-store type gen))

(define (arbitrary type)
  (hash-table-ref generator-store type
                  (lambda () (error "no generator found for" type))))

;; TODO: should probably make things seeded for determinism
;; TODO: change 10 to do sized stuff (see quickcheck)
(define arbitrary-integer
  (make-generator (lambda () (random 10))))
(add-generator 'integer arbitrary-integer)

(define arbitrary-boolean
  (make-generator (lambda () (= 0 (random 2)))))
(add-generator 'boolean arbitrary-boolean)


;; A shrinker is a generic handler that shrinks values of a given type.
;; It returns a list of all possible immediate shrinks of a given value.
(define shrink (simple-generic-procedure 'shrink 1 (lambda (a) (list))))

(define-generic-procedure-handler shrink (match-args integer?)
  (lambda (a)
    (list)))
