;;; blood-bind--extend.el -*- lexical-binding: t; no-byte-compile: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'blood-bind--vars)
  )

(defun bbe-add-kwd (type newkwd)
  "Add a new parse-time kwd"
  (cl-assert (symbolp type))
  (cl-assert (keywordp newkwd))
  )

(defun bbe-add-symbol-conversion (type sym kwd)
  "Add a new parse-time symbol -> kwd conversion"
  (cl-assert (symbolp type))
  (cl-assert (symbolp sym))
  (cl-assert (keywordp newkwd))

  )

(provide 'blood-bind--extend)
;; Local Variables:
;; read-symbol-shorthands: (
;; ("bbe-" . "blood-bind--extend-")
;; )
;; End:
;;; blood-bind--extend.el ends here
