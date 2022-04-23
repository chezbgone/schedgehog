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

(define (random-integer-in-range low high)
  (+ (random (- high low)) low))

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

