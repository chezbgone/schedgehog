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
(define (shrink-integer n)
  (delete-duplicates
   (cond
    ((> n 0) (list 0 (quotient n 2) (- n 1)))
    ((< n 0) (list 0 (quotient n 2) (+ n 1)))
    (else (list)))))

(define integer-gen
  (replace-shrinking shrink-integer
   (sized (lambda (size) (arbitrary `(range ,(- 1 size) ,size))))))
(set-generator! 'integer integer-gen)

(define positive-integer-gen
  (replace-shrinking (lambda (n) (delete 0 (shrink-integer n)))
   (sized (lambda (size) (arbitrary `(range 1 ,size))))))
(set-generator! 'positive-integer positive-integer-gen)

(define nonnegative-integer-gen
  (replace-shrinking shrink-integer
   (sized (lambda (size) (arbitrary `(linear ,size))))))
(set-generator! 'nonnegative-integer nonnegative-integer-gen)


;;; floats
(define (shrink-float x)
  (assert (real? x))
  (assert (not (exact-integer? x)))
  (let ((origin (round x)))
    (list (/ (+ x origin) 2))))

(define (float-gen size seed)
  (let ((generated-float (- (random (inexact (* 2 size)) seed) size)))
    (shrinkable-via shrink-float generated-float)))
(set-generator! 'float (%make-generator float-gen))


;;; lists
(define (lengthed-list-gen type len)
  (gen:sequence (make-list len (arbitrary type))))
(set-generator! 'lengthed-list lengthed-list-gen)

(define (list-gen type)
  (gen:then (arbitrary 'nonnegative-integer)
            (lambda (n) (lengthed-list-gen type n))))
(set-generator! 'list list-gen)

(define (nonempty-list-gen type)
  (gen:then (arbitrary 'positive-integer)
            (lambda (n) (lengthed-list-gen type n))))
(set-generator! 'nonempty-list nonempty-list-gen)


(define (gen:subsequence ls)
  (gen:map (lambda (bool-lst)
             (map cdr (filter car (map cons bool-lst ls))))
           (lengthed-list-gen 'boolean (length ls))))
(set-generator! 'subsequence gen:subsequence)

(define (gen:permutation ls)
  (if (null? ls)
      (gen:pure (list))
      (gen:then (arbitrary `(linear ,(length ls)))
                (lambda (idx)
                  (gen:map (lambda (sublist)
                             (cons (list-ref ls idx) sublist))
                           (gen:permutation ((lambda (lst idx)
                                               (append (take lst idx)
                                                       (drop lst (+ idx 1))))
                                             ls
                                             idx)))))))
(set-generator! 'permutation gen:permutation)


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
