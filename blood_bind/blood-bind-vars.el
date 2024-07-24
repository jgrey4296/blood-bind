;;; blood_bind_vars.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(defvar blood-bind--registry nil "where all bindings are stored, of type `blood-bind--store'")
(defvar blood-bind--generated nil "where bindings are assembled into a keymaps")
(defvar blood-bind--generation-hook nil "transforms to run on the keymap being assembled")
(defvar blood-bind--profiles nil "patterns that create a collection of binding keymaps")

(provide 'blood-bind-vars)

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
;;; blood_bind_vars.el ends here
