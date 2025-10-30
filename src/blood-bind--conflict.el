;;; blood-bind--conflict.el -*- lexical-binding: t; no-byte-compile: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'blood-bind--vars)
  )

(defun blood-bind--check-for-conflicts (coll) ;; -> list
  "Check a collection of binding entries for conflicts"
  (cl-assert (blood-bind--collection-p coll))

  (error "Not Implemented")
  )

(provide 'blood-bind--conflict)
;;; blood-bind--conflict.el ends here
