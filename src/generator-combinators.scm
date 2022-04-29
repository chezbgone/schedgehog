;; generator -> generator -> generator
(define (gen:pair genA genB)
  (define ((cons-curried a) b) (cons a b))
  (gen:app (gen:map cons-curried genA) genB))

;; list generator -> generator list
(define (gen:list . gens)
  (cond
   ((null? gens) (gen:pure (list)))
   (else         (gen:pair (car gens) (gen:sequence (cdr gens))))))

;; list generator -> generator list
(define (gen:sequence gens)
  (apply gen:list gens))

;; there is probably a better way of doing this
(define (gen:apply f . gens)
  (gen:map (lambda (seq) (apply f seq)) (gen:sequence gens)))

;; list a -> generator a
(define (gen:one-of ls)
  (gen:map (lambda (i) (list-ref ls i))
           (arbitrary `(linear ,(length ls)))))

;; list a -> generator a
;; no shrinking
(define (gen:one-of_ ls)
  (remove-shrinking (gen:one-of ls)))

;; list generators -> generator
(define (gen:select-from ls)
  (gen:then (arbitrary `(linear ,(length ls)))
            (lambda (idx)
              (list-ref ls idx))))

;; list (pair number a) -> generator a
(define (gen:one-of-with-frequencies ls)
  (gen:map (lambda (idx) (weighted-list-ref ls idx))
           (arbitrary `(linear ,(apply + (map car ls))))))

;; list (pair number a) -> generator a
(define (gen:select-from-with-frequencies ls)
  (gen:bind (arbitrary `(linear ,(apply + (map car ls))))
            (lambda (idx) (weighted-list-ref ls idx))))
