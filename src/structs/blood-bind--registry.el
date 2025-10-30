;;; blood-bind--registry.el -*- lexical-binding: t; no-byte-compile: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'seq)
  (require 'dash)
  (require 'blood-bind--vars)
  )

(defvar blood-bind--registry nil "where all bindings are stored, of type `blood-bind--store'")

(defvar blood-bind--type-kwds           (list :profile ))

(cl-defstruct (blood-bind--store
               (:constructor nil)
               (:constructor make-blood-bind--store)
               )
  " The main store of all profiles, pre- and post- compilation "
  (profiles     (make-hash-table)    :type 'hash-table)
  (collections  (make-hash-table)    :type 'hash-table)
  (transforms   (make-hash-table)    :type 'hash-table)
  (compiled     (make-hash-table)    :type 'hash-table)
  (partial-maps (make-hash-table)    :type 'hash-table)
  )

;;--------------------------------------------------


(provide 'blood-bind--registry)
;;; blood-bind--registry.el ends here
