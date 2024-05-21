;;; blood_bind_vars.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(defvar bloodbind--registry nil "where all bindings are stored, of type `bloodbind--store'")
(defvar bloodbind--generated nil "where bindings are assembled into a keymaps")
(defvar bloodbind--generation-hook nil "transforms to run on the keymap being assembled")
(defvar bloodbind--profiles nil "patterns that create a collection of binding keymaps")

(provide 'blood_bind_vars)

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
