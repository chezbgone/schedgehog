
;; A shrinker is a generic handler that shrinks values of a given type.
;; It returns a list of all possible immediate shrinks of a given value.
(define shrink (simple-generic-procedure 'shrink 1 (lambda (a) (list))))

(define-generic-procedure-handler shrink (match-args integer?)
  (lambda (a)
    (list)))
