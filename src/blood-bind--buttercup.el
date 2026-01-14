;;; extra-buttercup-matchers.el -*- lexical-binding: t; -*-

(require 'buttercup)


(buttercup-define-matcher :to-signal (expr &optional signal signal-args)
  "Defines :to-signal, which correctly catches cl-assert failures"
  (let ((expected-signal-symbol (and signal (funcall signal)))
        (expected-signal-args (and signal-args (funcall signal-args)))
        (unevaluated-expr (buttercup--enclosed-expr expr))
        (debug-on-error nil)
        expr-value
        thrown-signal)
    ;; Set the above variables
    (condition-case err
        (setq expr-value (funcall expr))
      (cl--assertion-failed
       (setq thrown-signal err)
       nil)
      (error
       (setq thrown-signal err)
       nil)
      )
    (buttercup--handle-to-throw thrown-signal
                                (cons expected-signal-symbol expected-signal-args)
                                unevaluated-expr expr-value)))

(buttercup-define-matcher :to-pass (expr test)
  "Check the expr's return value passes a bool test.
eg: cl-class-p
"
  (let ((actual-test (and test (funcall test))))
    (if (funcall actual-test (funcall expr))
        (cons t (format "Didn't expect to pass %s, but did: %s" actual-test (funcall expr)))
      (cons nil (format "Expected to pass %s, but didn't: %s" actual-test (funcall expr)))
        )
    )
  )

(buttercup-define-matcher :slot (expr type slot matcher &rest args)
  "Get the slot from a struct slot of the result of expr

Call as:
(expr) :slot 'the-type 'the-slot {test}...
 "
  (let* ((expr-result (funcall expr))
         (uneval-expr (buttercup--enclosed-expr expr))
         (slot-val (cl-struct-slot-value (funcall type)
                                         (funcall slot)
                                         expr-result))
         (slotw `(lambda () (quote ,uneval-expr) (quote ,slot-val)))
         (matcher (funcall matcher))
         )
    (buttercup--apply-matcher matcher (cons slotw args))
    )
)

(buttercup-define-matcher :has-length (expr val)
  " check a list has a length "
  (let ((ex-val (funcall expr)))
    (cons (and (sequencep ex-val)
               (eq (seq-length ex-val)
                   (funcall val)))
          (format "Expected length %s but got %s" (funcall val) (seq-length ex-val))
          )
    )
  )

(buttercup-define-matcher :plist-get (expr key matcher &rest args)
  "Get a value from a plist"
  (let* ((expr-result (funcall expr))
        (uneval-expr (buttercup--enclosed-expr expr))
        (val (plist-get expr-result (funcall key)))
        (valw `(lambda () (quote ,uneval-expr) (quote ,val)))
        (matcher (funcall matcher))
        )
    (buttercup--apply-matcher matcher (cons valw args))
    )
  )

(buttercup-define-matcher :car (expr matcher &rest args)
  "Get the car of a list"
  (let* ((expr-result (funcall expr))
         (uneval-expr (buttercup--enclosed-expr expr))
         (val (car-safe expr-result))
         (valw `(lambda () (quote ,uneval-expr) (quote ,val)))
         (matcher (funcall matcher))
         )
    (buttercup--apply-matcher matcher (cons valw args))
    )
  )


(provide 'blood-bind--buttercup)
;;; extra-buttercup-matchers.el ends here
