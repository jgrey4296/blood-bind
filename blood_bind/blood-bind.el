;;; blood_bind.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;; The public access api to blood-bind
;;
;;
;;-- end Header
(eval-when-compile
  (require 'cl-lib)
  )

(defconst blood-bind-version "0.1.0")

(cl-assert (<= 29 emacs-major-version))
(cl-assert (<= 3 emacs-minor-version))

(require 'blood-bind--util)
(require 'blood-bind--structs)
(require 'blood-bind--macros)
(require 'blood-bind--hooks)
(require 'blood-bind--compile)
(require 'blood-bind--reporter)

(defun blood-bind-apply (profile &optional merge)
  " apply a profile of compiled keymaps to non-blood-bind variables"
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
