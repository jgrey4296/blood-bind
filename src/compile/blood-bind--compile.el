;;; blood-bind--compile.el -*- lexical-binding: t; no-byte-compile: t; -*-

(eval-when-compile
  (require 'cl-lib)
  )


(defun blood-bind--compile-profile (profile) ;; profile -> list[keymap]

  )

(defun blood-bind--compile-isolated-collection (coll ctx) ;; collection -> ctx -> keymap
  ;; create matched keymap
  ;; compile each entry
  )

(defun blood-bind--compile-hook (coll ctx) ;; collection -> ctx -> lambda
  ;; for hooks, states, layers.

  )

(defun blood-bind--compile-entry (entry map ctx) ;; entry -> map -> ctx -> map'
  ;; check validity
  ;; insert into map
  )

(defun blood-bind--merge-keymaps (map1 map2 ctx) ;; map -> map -> ctx -> map

  )

(defun blood-bind--apply-parent (child parent ctx) ;; map -> map -> ctx -> map

  )

(defun blood-bind--all-maps-used-or-partial-p (profile) ;; profile -> bool

  )



(provide 'blood-bind--compile)
;;; blood-bind--compile.el ends here
