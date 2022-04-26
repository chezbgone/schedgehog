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

(define (integer-gen size seed)
  (let ((generated-integer (- (random (- (* 2 size) 1) seed) (- n 1))))
    (shrinkable-via shrink-integer generated-integer)))
(set-generator! 'integer (%make-generator integer-gen))

(define (nonnegative-integer-gen size seed)
  (shrinkable-via shrink-integer (random size seed)))
(set-generator! 'nonnegative-integer (%make-generator nonnegative-integer-gen))


;;; lists
(define (list-gen type)
  (gen:then (arbitrary 'nonnegative-integer)
            (lambda (n)
              (gen:sequence (make-list n (arbitrary type))))))


;; TODO
;; gen:subsequence
;; gen:permutation


;; CODE BELOW IS PROBABLY ALL WRONG
#|

(define arbitrary-uppercase-character
  (make-generator (lambda () (integer->char (+ (char->integer #\A)
                                               (random 26))))))
(add-generator 'uppercase-character arbitrary-uppercase-character)

(define arbitrary-lowercase-character
  (make-generator (lambda () (integer->char (+ (char->integer #\a)
                                               (random 26))))))
(add-generator 'lowercase-character arbitrary-lowercase-character)

(define arbitrary-letter
  (make-generator (lambda () (if (= (random 2) 0)
                                 ((generate arbitrary-lowercase-character))
                                 ((generate arbitrary-uppercase-character))))))
(add-generator 'letter arbitrary-letter)

(define arbitrary-numeric-character
  (make-generator (lambda () (integer->char (+ (char->integer #\0)
                                               (random 10))))))
(add-generator 'numeric-character arbitrary-numeric-character)

(define arbitrary-alphanumeric-character
  (make-generator (lambda () (if (> (random 62) 9)
                                 ((generate arbitrary-letter))
                                 ((generate arbitrary-numeric-character))))))
(add-generator 'alphanumeric-character arbitrary-alphanumeric-character)

(define arbitrary-character
  (make-generator (lambda () (integer->char (random char-code-limit)))))
(add-generator 'character arbitrary-character)

;; Generates a random string of a fixed length with characters of type character-type. Character-type must be specified, otherwise printing out characters might result in strings longer than intended.
(define (arbitrary-string length character-type)
  (if (<= length 1)
      (char->name (arbitrary-character character-type))
      (string-append (char->name (arbitrary-character character-type))
                 (arbitrary-string (- length 1) character-type))))
|#
