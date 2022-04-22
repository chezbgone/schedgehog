;; A state represents the internal state when testing a property.

(define-record-type state
  (make-state successful-tests max-successful-tests)
  state?
  (successful-tests state-successful-tests set-state-successful-tests!)
  (max-successful-tests state-max-successful-tests
                        set-state-max-successful-tests!))

;; Main state machine.
;; Run a test on property.
(define (test state property)
  (define res (property-assertion (map gen (property-types))))
  (and res
    (set-state-successful-tests! state (+ 1 (successful-tests state))))
  'done)
