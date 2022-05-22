(load "schedgehog.scm")

(define default-config (make-config 1000 100))

(define prop:addition-commutativity
  (forall ((x integer) (y integer))
          (= (+ x y) (+ y x))))
(check prop:addition-commutativity)

(define (flip pair)
  (cons (cdr pair)
        (car pair)))

(define prop:flip-twice-is-id
  (forall ((p (pair integer boolean)))
          (equal? p (flip (flip p)))))
(check prop:flip-twice-is-id)

(define (generate-random-bounded-list len low high)
  (if (= len 0)
      '()
      (cons (+ low (random (- high low)))
            (generate-random-bounded-list (- len 1)
                                          low
                                          high))))

(define prop:bounded-list-is-within-range
  (forall ((len positive-integer) (low integer) (diff positive-integer))
          ((lambda (ls)
             (let loop ((ls ls))
               (if (pair? ls)
                   (and (>= (car ls) low)
                        (< (car ls) (+ low diff))
                        (loop (cdr ls))))))
           (generate-random-bounded-list len low (+ low diff)))))

;; below code has a "fencepost error"
(define (broken-generate-random-bounded-list len low high)
  (if (= len 0)
      '()
      (cons (+ low (random (+ (- high low) 1)))
            (generate-random-bounded-list (- len 1)
                                          low
                                          high))))

(define prop:bounded-list-is-within-range-broken
  (forall ((len positive-integer) (low integer) (diff positive-integer))
          ((lambda (ls)
             (let loop ((ls ls))
               (if (pair? ls)
                   (and (>= (car ls) low)
                        (< (car ls) (+ low diff))
                        (loop (cdr ls))))))
           (broken-generate-random-bounded-list len low (+ low diff)))))
(check prop:bounded-list-is-within-range-broken)













; =======================
(load "schedgehog.scm")

(define prop:reverse-append
  (forall ((xs (list integer))
           (ys (list integer)))
    (equal? (reverse (append xs ys))
            (append (reverse ys)
                    (reverse xs)))))

(define (reverse xs)
  (if (null? xs)
      (list)
      (append (reverse (cdr xs))
              (list (car xs)))))

(check prop:reverse-append)



















(define (reverse xs)
  (case (length xs)
    ((0) (list))
    ((1) (car xs))
    (else (cons (car xs)
                (reverse (cdr xs))))))
