;; lazy-tree T : {
;;   value : T
;;   children : list (() -> lazy-tree T)
;; }
(define-record-type lazy-tree
  (make-lazy-tree value children)
  lazy-tree?
  (value lazy-tree-value)
  (children lazy-tree-children))

(define-record-type tree
  (make-tree value children)
  tree?
  (value tree-value)
  (children tree-children))

(define (force-tree tr)
  (make-tree (lazy-tree-value tr)
             (map force-lazy-tree (lazy-tree-children tr))))

(define (print-tree tree)
  (define (%print-tree prefix is-last t)
    (display prefix)
    (display "+- ")
    (display (tree-value t))
    (newline)
    (if (not (null? (tree-children t)))
        (let ((new-prefix (string-append prefix (if is-last "   " "|  "))))
          (for-each (lambda (child) (%print-tree new-prefix #f child))
                    (drop-right (tree-children t) 1))
          (%print-tree new-prefix #t (last (tree-children t))))))
  (%print-tree "" #t tree))

(define (print-lazy-tree tree)
  (print-tree (force-tree tree)))
