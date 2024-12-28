;;; blood_bind_reporter.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(defun bbr-conflict (&rest conflicts)
  " report on conflicting patterns for debugging "

 )

(defun bbr-overview-profiles ()
  "Report on all registered profiles"

  )

(defun bbr-overview-entries ()
  "report on all entries"

  )

(defun bbr-overview-transforms ()
  "report on all registered transforms"

  )

(defun bbr-overview-maps ()
  "report on all maps bb has control over"
  )

(defun bbr-dsl ()
  "Generate a description of the DSL"
  )

(provide 'blood-bind--reporter)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    April 03, 2024
;; Modified:   April 03, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; blood_bind_reporter.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("bbr-" . "blood-bind--reporter-")
;; )
;; End:
