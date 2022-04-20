;; A generator is a generic handler that generates values of a given type.
(define gen (simple-generic-procedure 'gen 1 #f))

(define-generic-procedure-handler gen (match-args integer?)
  (lambda (a)
    0))

;; A shrinker is a generic handler that shrinks values of a given type.
;; It returns a list of all possible immediate shrinks of a given value.
(define shrink (simple-generic-procedure 'shrink 1 (lambda (a) (list))))

(define-generic-procedure-handler shrink (match-args integer?)
  (lambda (a)
    (list)))
