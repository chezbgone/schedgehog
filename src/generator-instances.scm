
;;; booleans



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
  (shrinkable-via shrink-integer (random size seed)))

(set-generator! 'integer
               (%make-generator integer-gen))
