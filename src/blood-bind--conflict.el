;;; blood-bind--conflict.el -*- lexical-binding: t; no-byte-compile: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'blood-bind--vars)
  (require 'blood-bind--structs)
  )

(defun bbc-check-for-conflicts (coll) ;; -> list
  "Check a collection of binding entries for conflicts"
  (cl-assert (blood-bind--collection-p coll))

  (error "Not Implemented")
  )

(provide 'blood-bind--conflict)
;; Local Variables:
;; read-symbol-shorthands: (
;; ("bbc-" . "blood-bind--conflict-")
;; )
;; End:
;;; blood-bind--conflict.el ends here
