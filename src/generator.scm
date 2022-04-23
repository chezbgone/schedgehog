;; A generator is a function that returns
;; an arbitrary element of the type it represents.
(define-record-type generator
  (make-generator generate)
  generator?
  (generate generate))

(define generator-store (make-hash-table))
(define (add-generator type gen)
  (hash-table-set! generator-store type gen))

(define (arbitrary type)
  (hash-table-ref generator-store type
                  (lambda () (error "no generator found for" type))))

;; TODO: should probably make things seeded for determinism
;; TODO: change 10 to do sized stuff (see quickcheck)
(define arbitrary-integer
  (make-generator (lambda () (random 10))))
(add-generator 'integer arbitrary-integer)

(define arbitrary-boolean
  (make-generator (lambda () (= 0 (random 2)))))
(add-generator 'boolean arbitrary-boolean)

;; Generates a random character of type 'alpha, 'numeric, 'alphanumeric, or just any character. Alphanumeric characters are letters with probability 50% and numbers with probability 50%.
(define (arbitrary-character #!optional character-type)
  (assert (or (member character-type (list 'alpha
                                           'numeric
                                           'alphanumeric))
              (default-object? character-type)))
  (cond
   ((equal? character-type 'alpha)
    (let ((char-number (random 52)))
      (if (> char-number 25)
          (integer->char (+ 39 char-number))
          (integer->char (+ 97 char-number)))))
   ((equal? character-type 'numeric)
    (integer->char (+ (random 10) 48)))
   ((equal? character-type 'alphanumeric)
    (if (= (random 2) 0)
        (arbitrary-character 'alpha)
        (arbitrary-character 'numeric)))
   (else
    (integer->char (random char-code-limit)))))

;; Generates a random string of a fixed length with characters of type character-type. Character-type must be specified, otherwise printing out characters might result in strings longer than intended.
(define (arbitrary-string length character-type)
  (if (<= length 1)
      (char->name (arbitrary-character character-type))
      (string-append (char->name (arbitrary-character character-type))
                 (arbitrary-string (- length 1) character-type))))

