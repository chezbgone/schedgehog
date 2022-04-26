;; A state represents the internal state when testing a property.
(define-record-type state
  (make-state successful-tests max-successful-tests)
  state?
  (successful-tests state-successful-tests set-state-successful-tests!)
  (max-successful-tests state-max-successful-tests))

(define-record-type failure
  (make-failure shrinks inputs)
  failure?
  (shrinks failure-shrinks)
  (inputs failure-arguments))

; recursively shrink a failing arguments-tree
(define (shrink-failure shrinks assertion arguments-tree)
  (define (loop children)
    (if (null? children) #f
      (let* ((new-tree ((car children)))
             (arguments (lazy-tree-value new-tree))
             (result (apply assertion arguments)))
        (if result (loop (cdr children)) new-tree))))
  (let ((result (loop (lazy-tree-children arguments-tree))))
    (if result
      (shrink-failure (+ 1 shrinks) assertion result)
      (make-failure shrinks (lazy-tree-value arguments-tree)))))

; checks a property. if it passes, returns #t; otherwise returns failing
; arguments after shrinking.
(define (check-once property)
  (let* ((types (property-types property))
         (assertion (property-assertion property))
         (arguments-generator (gen:sequence (map arbitrary types)))
         (arguments-tree (generate arguments-generator))
         (arguments (lazy-tree-value arguments-tree))
         (result (apply assertion arguments)))
    (if result
      #t
      (shrink-failure 0 assertion arguments-tree))))

#|
(define prop:addition-commutativity
  (forall ((x integer) (y integer))
          (= (+ x y) (+ y x))))
(check-once prop:addition-commutativity)
|#

#|
(define (flip pair)
  (cons (cdr pair)
        (car pair)))

(define prop:flip-twice-is-id
  (forall ((p (pair integer boolean)))
          (equal? p (flip (flip p)))))
(check-once prop:flip-twice-is-id)
|#

#|
(define prop:sum-linear
  (forall ((xs (list integer)) (factor integer))
          (equal? (* factor (apply + xs))
                  (apply + (map (lambda (x) (* factor x)) xs)))))
(check-once prop:sum-linear)
|#
