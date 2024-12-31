;;; blood-bind--conflict.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

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

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    December 31, 2024
;; Modified:   December 31, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("bbc-" . "blood-bind--conflict-")
;; )
;; End:
;;; blood-bind--conflict.el ends here
