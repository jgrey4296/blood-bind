;;; blood-bind--transforms.el -*- lexical-binding: t; no-byte-compile: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'seq)
  (require 'dash)
  (require 'blood-bind--vars)
  )

(cl-defstruct (blood-bind--transform
               (:constructor nil)
               (:constructor make-blood-bind--transform))
  "Describes a registered transform "
  (pattern     nil :type 'blood-bind--pattern        :read-only t)
  (replacement nil :type (or 'symbol 'pattern) :read-only t)
  (sort        nil :type 'int                        :read-only t)
  (source      nil :type 'str                        :read-only t)
  )

;;--------------------------------------------------

(defun make-blood-bind-transforms (source raw) ;; -> plist(:entries list[transform])
  "build a transfrom description"
  (let (transforms
        )
    (while raw
      (pcase (seq-first raw)
        ((and x (pred blood-bind--transform-p))
         (push x transforms)
         (setq raw (seq-rest raw)))
        ((pred vectorp)
         (cl-destructuring-bind (patt op target) (seq-take raw 3)
           (push (make-blood-bind--transform
                  :pattern (blood-bind--parse-pattern patt)
                  :replacement (pcase target
                                 ((pred vectorp)
                                  (blood-bind--parse-pattern target))
                                 ((pred symbolp)
                                  target)
                                 )
                  :source source
                  )
                 transforms)
           )
         (setq raw (seq-drop raw 3))
         )
        (x (signal 'blood-bind-parse-error 'transforms x))
        )
      )
    (list :entries (reverse transforms))
    )
  )

(provide 'blood-bind--transforms)
;;; blood-bind--transforms.el ends here
