
;;; booleans
(set-generator! 'boolean (gen:one-of (list #t #f)))

;;; integers

(define (shrink-integer n)
  (delete-duplicates
   (cond
    ((> n 0) (list (- n 1) (quotient n 2) 0))
    ((< n 0) (list (+ n 1) (quotient n 2) 0))
    (else (list)))))

(define (arbitrary-integer size seed)
  (random size seed))

(define (integer-gen size seed)
  (shrinkable-via shrink-integer (arbitrary-integer size seed)))

(set-generator! 'integer (%make-generator integer-gen))


;;; ranges
(define ((int:shrink-towards to) k)
  (cond
   ((= k to) (list))
   ((< k to) (list (+ k 1)))
   ((> k to) (list (- k 1)))))

;; arbitrary integer from [m .. (n-1)], shrinking towards `to`
(define ((range-from-gen m n to) size seed)
     (shrinkable-via (int:shrink-towards to)
                     (+ (random (- n m) seed) m)))
(set-generator! 'range-from
                (lambda (m n to)
                  (%make-generator (range-from-gen m n to))))

;; arbitrary integer from [m .. (n-1)], shrinking down
(set-generator! 'range
                (lambda (m n) (arbitrary `(range-from ,m ,n ,m))))

;; arbitrary integer from [0 .. (n-1)], shrinking down
(set-generator! 'linear
                (lambda (n) (arbitrary `(range 0 ,n))))
