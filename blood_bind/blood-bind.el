;;; blood_bind.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(defconst blood-bind-version "0.1.0")

(defun blood-bind-apply (profile &optional merge)
  " apply compiled keymaps to non-blood-bind variables"
  (interactive)
  ;; retrieve keymaps from registry
  ;; set their respective variables
  )

(defun blood-bind-compile (&rest profiles)
  " Run the compilation of specified profiles
profile -> list[entry] -> list[mapname]
create {name -> profile-local-name} mapping, and create profile maps
for entry in entries: bind target to correct profile map

inserting compiled-profile structs into the store ready to apply

"
  (interactive)
  )

(defun blood-bind-summary (&optional profile map state pattern)
  " summarise bindings according to a pattern "
  (interactive)
  )

(defun blood-bind-wipe (&optional profile map state)
  " wipe all bindings in all keymaps "
  (interactive)

  )

(defun blood-bind-disable-interference ()
  " disable non-blood-bind means of binding keys "
  (interactive)

  )

(defun blood-bind-register-interference (&rest args)
  " register a means that other modes interfere with keybindings "

  )

(provide 'blood-bind)

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
