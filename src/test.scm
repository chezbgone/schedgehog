;; Configuration for a given run.
;;    shrink-limit:   max number of shrinks before giving up
;;    test-limit:     max number of tests to run
;;        TODO: change test-limit to generalized termination criteria
(define-record-type config
  (make-config shrink-limit test-limit)
  config?
  (shrink-limit config-shrink-limit)
  (test-limit config-test-limit))

;; A failure record.
;;    shrinks: number of shrinks to produce this failure
;;    inputs:  list of arguments that produce the failure
(define-record-type failure
  (make-failure shrinks inputs)
  failure?
  (shrinks failure-shrinks)
  (inputs failure-arguments))

;; Recursively shrink a node that fails a given assertion.
;; config -> number -> (a -> bool) -> a -> failure
(define (shrink-failure config shrinks assertion node)
  (define (loop children tries)
    (if (null? children)
      (cons #f tries) ; ran out of children to try
      (let* ((new-node ((car children)))
             (arguments (lazy-tree-value new-node))
             (result (apply assertion arguments)))
        (if (not result)
          (cons new-node tries) ; new-node is shrinked, return it
          (loop (cdr children) (+ tries 1))))))
  (if (>= shrinks (config-shrink-limit config))
    (make-failure shrinks (lazy-tree-value node))
    (let* ((result (loop (lazy-tree-children node) 0))
           (new-node (car result))
           (tries (cdr result))
           (new-shrinks (+ shrinks tries)))
      (if new-node
        (shrink-failure config new-shrinks assertion new-node)
        (make-failure new-shrinks (lazy-tree-value node))))))

;; Runs a single test.
;; config -> number -> random-state -> property -> #t | failure
(define (check-once config size seed property)
  (let* ((types (property-types property))
         (assertion (property-assertion property))
         (arguments-generator (gen:sequence (map arbitrary types)))
         (arguments-tree (generate arguments-generator size seed))
         (arguments (lazy-tree-value arguments-tree))
         (result (apply assertion arguments)))
    (cond
      ((eq? result 'condition-not-satisfied) result)
      ((not result) (shrink-failure config 0 assertion arguments-tree))
      (else #t))))

;; Runs multiple tests.
;; config -> number -> random-state -> property -> #t | failure
(define (check-with-config config size seed property)
  (define tries 0)
  (define discards 0)
  (define (loop)
    (set! tries (+ tries 1))
    (if (>= tries (config-test-limit config))
      (begin ; we succeeded
        (if (> discards 0)
          (begin
            (display "discarded ")
            (display discards)
            (display " tests")
            (newline)))
        (display "ok ")
        (display tries)
        (display " tests")
        (newline)
        #t)
      (let ((result (check-once config size seed property)))
        (cond
          ((eq? result 'condition-not-satisfied)
           (begin
             (set! discards (+ discards 1))
             (loop)))
          ((failure? result)
            (begin ; we failed
              (display "failed after ")
              (display tries)
              (display " tests, ")
              (display (failure-shrinks result))
              (display " shrinks:")
              (newline)
              (display (zip (property-vars property) (failure-arguments result)))
              (newline)
              result))
          (else (loop))))))
  (loop))

(define default-config (make-config 1000 100))

;; Checks a property, optionally specifying size and seed
(define (check property #!optional size seed)
  (let ((sz (if (default-object? size) default-size size))
        (sd (if (default-object? seed) (make-random-state #t) seed)))
    (display "; seed: ")
    (display (export-random-state sd))
    (newline)
    (check-with-config default-config sz sd property)))


#|
(define prop:addition-commutativity
  (forall ((x integer) (y integer))
          (= (+ x y) (+ y x))))
(check prop:addition-commutativity)
|#

#|
(define (flip pair)
  (cons (cdr pair)
        (car pair)))

(define prop:flip-twice-is-id
  (forall ((p (pair integer boolean)))
          (equal? p (flip (flip p)))))
(check prop:flip-twice-is-id)
|#

#|
(define prop:sum-linear
  (forall ((xs (list integer)) (factor integer))
          (equal? (* factor (apply + xs))
                  (apply + (map (lambda (x) (* factor x)) xs)))))
(check prop:sum-linear)
|#
