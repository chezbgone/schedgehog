;; A property is a function with typed arguments returning a bool.

(define-record-type property
  (make-property types predicate)
  property?
  (types property-types)
  (predicate property-predicate))

(define-syntax forall
  (syntax-rules ()
    ((_ ((var type) ...) body)
     (make-property '(type ...)
                     (lambda (var ...) body)))))

;; example usage:
#|
(define prop:addition-commutativity
  (forall ((x integer?) (y integer?))
          (= (+ x y) (+ y x))))

(property-types prop:addition-commutativity)
(property-predicate prop:addition-commutativity)
|#
