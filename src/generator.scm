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

