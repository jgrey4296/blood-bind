;;; blood_bind.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(defconst blood-bind-version "0.1.0")

(defun bloodbind-apply (profile)
  " apply compiled keymaps to non-bloodbind variables"
  (interactive)
  ;; retrieve keymaps from registry
  ;; set their respective variables
  )

(defun bloodbind-compile ()
  " generate the bindings that will match an emacs state pattern "
  (interactive)
  ;; expand all registered bind patterns
  ;; transform bind patterns
  ;; assemble into submaps
  ;; transform submaps
  ;; assemble submaps into full maps
  ;; assemble maps into profiles

  )



(defun bloodbind-summary (&rest args)
  " summarise bindings according to a pattern "
  (interactive)
  )

(defun bloodbind-wipe ()
  " wipe all bindings in all keymaps "
  (interactive)

  )

(defun bloodbind-disable-interference ()
  " disable non-bloodbind means of binding keys "
  (interactive)

  )

(defun bloodbind-register-interference (&rest args)
  " register a means that other modes interfere with keybindings "

  )

(provide 'blood_bind)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    February 08, 2024
;; Modified:   February 08, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; blood_bind.el ends here
