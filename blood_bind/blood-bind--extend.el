;;; blood-bind--extend.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;; api for adding elements to blood-bind
;;
;;-- end Header

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

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    December 23, 2024
;; Modified:   December 23, 2024
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
;; ("bbe-" . "blood-bind--extend-")
;; )
;; End:
;;; blood-bind--extend.el ends here
