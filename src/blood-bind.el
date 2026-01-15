;;; blood-bind.el --- A keybinding framework -*- lexical-binding: t; -*-

;; Copyright (C) 2025 john
;;
;; Author: john <https://github.com/jgrey4296>
;; Created: October 29, 2025
;; Modified: October 29, 2025
;; Version: 0.1.0
;; Keywords:
;; Homepage: https://github.com/jgrey4296/blood-bind
;; Package-Requires: ((emacs "30.2") (dash) (macro-tools))
;; Package written on: ((emacs 30.2))
;;
;; This file is not part of GNU Emacs.
;;

;;; Commentary:

(eval-when-compile
  (require 'cl-lib)
  )

(defconst blood-bind-version "0.1.0")

(cl-assert (<= 29 emacs-major-version))

(require 'blood-bind--util)
(require 'blood-bind-structs)
(require 'blood-bind--macros)
(require 'blood-bind--hooks)
(require 'blood-bind--reporter)
(require 'blood-bind--compile)

(defun blood-bind-apply (&rest profiles)
  "apply profiles of compiled keymaps to non-blood-bind variables"
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

if profiles is nill, compile all registered profiles
"
  (interactive)
  )

(cl-defun blood-bind-summary (&key profile map state pattern)
  " summarise bindings according to a pattern "
  (interactive)
  )

(cl-defun blood-bind-wipe (&key pattern map profile transform)
  "wipe all registered patterns/maps/profiles/transforms"
  (interactive)

  )

(defun blood-bind-disable-interference ()
  " disable non-blood-bind means of binding keys "
  (interactive)

  )

(defun blood-bind-register-interference (&rest args)
  " register a means that other modes interfere with keybindings "

  )

;; autoloads  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'blood-bind)
;;; blood_bind.el ends here
