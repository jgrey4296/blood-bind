;;; blood-bind--profile.el -*- lexical-binding: t; no-byte-compile: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'seq)
  (require 'dash)
  (require 'blood-bind--vars)
  )

(cl-defstruct (blood-bind--profile
               (:constructor nil)
               (:constructor make-blood-bind--profile)
               )
  "A named collection of collections."
  (name        nil  :type 'symbol :read-only t)
  (docstr      nil  :type 'string :read-only t)
  (source      nil  :type 'string :read-only t)
  (collections nil  :type 'list   :read-only t)
  )

;;--------------------------------------------------

(defun make-blood-bind-profile (name docstr source collections)
  ""
  )

(defun blood-bind--profile-mapnames (profile) ;; profile -> list[symbol]


  )




(provide 'blood-bind--profile)
;;; blood-bind--profile.el ends here
