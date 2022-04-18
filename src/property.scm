;; A property is a function with typed arguments returning a bool.

(define-record-type property
  (make-predicate types predicate)
  property?
  (types property-types)
  (predicate property-predicate))

(define-syntax forall
  (syntax-rules ()
    ((_ ((var type) ...) body)
     (make-predicate '(type ...)
                     (lambda (var ...) body)))))

;; example usage:
#|
(define integer-addition-commutativity
        (forall ((x integer) (y integer))
                ((+ x y) (+ y x))))

(property-types integer-addition-commutativity)
(property-types addition-commutativity)
|#
