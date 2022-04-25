;;; ranges
(define ((int:shrink-towards to) k)
  (cond
   ((= k to) (list))
   ((< k to) (list (+ k 1)))
   ((> k to) (list (- k 1)))))

;; arbitrary integer from [m .. (n-1)], shrinking towards `origin`
(define ((range-from-gen origin m n) size seed)
     (shrinkable-via (int:shrink-towards origin)
                     (+ (random (- n m) seed) m)))
(set-generator! 'range-from
                (lambda (origin m n)
                  (%make-generator (range-from-gen origin m n))))

;; arbitrary integer from [m .. (n-1)], shrinking towards the first argument
(set-generator! 'range
                (lambda (m n)
                  (if (> m n)
                      (arbitrary `(range-from ,m ,n ,m))
                      (arbitrary `(range-from ,m ,m ,n)))))

;; arbitrary integer from [0 .. (n-1)], shrinking downwards
(set-generator! 'linear
                (lambda (n) (arbitrary `(range 0 ,n))))

;;; booleans
(set-generator! 'boolean (gen:one-of (list #f #t)))
(set-generator! 'boolean_ (gen:one-of_ (list #f #t)))


;;; integers
;; todo: deal with positive/negative stuff
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
