;; list (pair number a) -> number -> a
(define (weighted-list-ref ls idx)
  (assert (not (null? ls))
          "weighted-list-ref: index too high")
  (if (< idx (car (first ls)))
      (cdr (first ls))
      (weighted-list-ref (cdr ls)
                         (- idx (car (first ls))))))
