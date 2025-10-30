;;; blood-bind--profile.el -*- lexical-binding: t; no-byte-compile: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'seq)
  (require 'dash)
  (require 'blood-bind--vars)
  )

(defvar blood-bind--profiles nil "patterns that create a collection of binding keymaps")

;;--------------------------------------------------


(provide 'blood-bind--profile)
;;; blood-bind--profile.el ends here
