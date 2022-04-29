(load "../src/tree.scm")
(load "../src/generator.scm")
(load "../src/generator-instances.scm")

;;; Integers

;; We can generate an arbitrary integer, and it gives us a lazy-tree.
;; The value of the integer is the root of the tree:
(generate (arbitrary 'integer))
(lazy-tree-value (generate (arbitrary 'integer)))

;; We can specify the size of the integer, which is 20 by default.
;; For integers, the size represents max absolute value:
(lazy-tree-value (generate (arbitrary 'integer) 5))

;; The actual tree structure recursively gives all of the options for shrinking.
;; `print-lazy-tree` forces the lazy tree to be strict, and then prints it.
;; For an integer n, it can shrink to (n - 1), (n / 2) or 0.
(print-lazy-tree (generate (arbitrary 'integer) 5))

;;; Booleans
;; Booleans shrink to false.
(print-lazy-tree (generate (arbitrary 'boolean)))
;; There is a non-shrinking version
(print-lazy-tree (generate (arbitrary 'boolean_)))


;;; Ranges

;; Ranges are different from integers because they represent a sequence,
;; as opposed to a numerical value. The biggest difference is how they shrink:
;; a number in a range typically shrinks towards some "origin".

;; (linear n) is an integer from [0 .. (n - 1)], shrinking downwards:
(print-lazy-tree (generate (arbitrary '(linear 10))))

;; (range m n) is an integer from [m .. (n - 1)],
;; shrinking towards the first argument:
(print-lazy-tree (generate (arbitrary '(range 10 15))))
(print-lazy-tree (generate (arbitrary '(range 15 10))))
;; Notice that we can't shrink past the bound!
;; This is important because if we only want to test values between 10 and 15,
;; then shrinking should not break this invariant.

;; (range-from origin m n) is an integer from [m .. (n - 1)],
;; shrinking towards origin:
(print-lazy-tree (generate (arbitrary '(range-from 15 10 20))))
;; This may be useful for dates, e.g. (range-from 2000 1970 2100)


;;; combinators

;; constant-gen is a generator that always returns the same thing.
;; This is also called gen:pure for algebraic reasons.
(print-lazy-tree (generate (constant-gen 4)))
(print-lazy-tree (generate (constant-gen (list 'a 3 #t))))

;; gen:one-of takes in a list, and returns a generator that
;; uniformly selects an element of that list.
;; Shrinks towards earlier elements.
(print-lazy-tree (generate (gen:one-of (list 100 200 250))))
;; There is a non-shrinking version, typically used when
;; the items in the list are not of the same type
(print-lazy-tree (generate (gen:one-of_ (list 3 'a "hello"))))

;; gen:map applies a function to generated values:
(define (times-two x) (* x 2))
(print-lazy-tree (generate (gen:map times-two (arbitrary 'integer)) 5))
;; Notice that the shrink values also are mapped!

;; gen:pair takes two generators and returns a generator that generates a pair:
(let ((generator (gen:pair (arbitrary '(range-from 0 -1 5))
                           (arbitrary 'boolean))))
  (newline)
  (print-lazy-tree (generate generator)))
;; Note that it interleaves shrinking of both coordinates.

;; gen:list takes in an arbitrary number of generators and generates a list:
(let ((generator (gen:list (arbitrary '(range-from 0 -2 2))
                           (arbitrary 'boolean)
                           (gen:one-of (list 1337 350)))))
  (newline)
  (print-lazy-tree (generate generator)))

;; gen:sequence is similar.
;; It takes in a list of generators, and generates a list
(let* ((gens (list (arbitrary '(linear 2))
                   (arbitrary 'boolean)
                   (gen:one-of (list "a" "b"))))
       (generator (gen:sequence gens)))
  (newline)
  (print-lazy-tree (generate generator)))


;; gen:apply is similar to `apply`, but it applies
;; the function to the generated values
;; The following generates a random rational a/b where
;; a ∈ [5 .. 9] and b ∈ [1 .. 4],
;; and shrinking by decreasing the numerator and denominators.
(let* ((generator (gen:apply / (arbitrary '(range 5 10))
                               (arbitrary '(range 1 5)))))
  (newline)
  (print-lazy-tree (generate generator)))
