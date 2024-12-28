;;; blood_bind_compile.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(defun bbc-profile (profile) ;; bbs-profile -> bbs-profile-compiled
  "Compiles a named profile with the segments it describes"
  ;; get profile
  ;; get global bindings
  ;; extract map-vars
  ;; build empty compiled-profile of maps
  ;; for (locals, entries) in profile:
  ;; ;; for entry in entries:
  ;; ;; ;; expand-entry(entry, using=locals, onto=comp-profile.lookup[entry.map])

  ;; return compiled profile
  )

(defun bbc-entry (entry locals globals maps) ;; bbs-entry -> list
  "Convert an entry into a keymap binding"
  ;; get maps[entry.pattern.map|state] -> profilemap
  ;; entry.pattern.keys -> keymap vector
  ;; bind pmap[keyvec] = maps[entry.target] | entry.target
  )

(defun bbc-gen-maps () ;; -> hash-table[sym, keymap]
  "Generate the empty keymaps blood-bind will put bindings into"
  )

(provide 'blood-bind--compile)

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
;;; blood_bind_compile.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("bbc-" . "blood-bind--compile-")
;; )
;; End:
