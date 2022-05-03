;; A property is a function with typed arguments returning a bool.

(define-record-type property
  (make-property vars types assertion)
  property?
  (vars property-vars)
  (types property-types)
  (assertion property-assertion))

(define-syntax try
  (syntax-rules ()
    ((_ f)
     (let ((result (ignore-errors (lambda () f))))
       (if (condition? result) #f result)))))

(define-syntax forall
  (syntax-rules ()
    ((_ ((var type) ...) body ...)
     (make-property '(var ...)
                    '(type ...)
                    (lambda (var ...) (try body ...))))))

(define-syntax when
  (syntax-rules ()
    ((_ consequent) consequent)
    ((_ antecedent consequent ...)
     (let ((ant (ignore-errors (lambda () antecedent))))
       (if (or (condition? ant) (not ant))
           'condition-not-satisfied
           (when consequent ...))))))

#|
;; example usage:
(define prop:addition-commutativity
  (forall ((x integer) (y integer))
          (= (+ x y) (+ y x))))

(property-vars prop:addition-commutativity)
(property-types prop:addition-commutativity)
(property-assertion prop:addition-commutativity)
|#
