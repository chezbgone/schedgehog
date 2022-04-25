;; lazy-tree T : {
;;   value : T
;;   children : list (() -> lazy-tree T)
;; }
(define-record-type lazy-tree
  (make-lazy-tree value children)
  lazy-tree?
  (value lazy-tree-value)
  (children lazy-tree-children))

;;; tree utils

(define ((%lazy-tree:map f) tr)
  (let* ((value (lazy-tree-value tr))       ; a
         (children (lazy-tree-children tr)) ; list (() -> lazy-tree a)
         (map-child                         ; (() -> lazy-tree a) -> (() -> lazy-tree b)
          (lambda (lt) (lambda ()
            ((%lazy-tree:map f) (lt)))))
         (mapped-children (map map-child children))) ; list (() -> lazy-tree b)
    (make-lazy-tree (f value)
                    mapped-children)))

;; (a -> b) -> (lazy-tree a -> lazy-tree b)
(define (lazy-tree:map f tr)
  ((%lazy-tree:map f) tr))


(define-record-type tree
  (make-tree value children)
  tree?
  (value tree-value)
  (children tree-children))

;; lazy-tree a -> tree a
(define (force-tree tr)
  (make-tree (lazy-tree-value tr)
             (map (lambda (subtree) (force-tree (subtree)))
                  (lazy-tree-children tr))))

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