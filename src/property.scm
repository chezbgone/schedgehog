;; A property is a function with typed arguments returning a bool.

(define-record-type property
  (make-property vars types assertion)
  property?
  (vars property-vars)
  (types property-types)
  (assertion property-assertion))

(define-syntax forall
  (syntax-rules ()
    ((_ ((var type) ...) body)
     (make-property '(var ...)
                    '(type ...)
                    (lambda (var ...) body)))))

#|
;; example usage:
(define prop:addition-commutativity
  (forall ((x integer) (y integer))
          (= (+ x y) (+ y x))))

(property-vars prop:addition-commutativity)
(property-types prop:addition-commutativity)
(property-assertion prop:addition-commutativity)
|#
