;;; compile-time-check.el -*- lexical-binding: t; no-byte-compile: t; -*-

(bloodbind! compile-check ()
  "Define patterns that run a check after compilation"
  ;; Require this to be overriden:
  [a b c] -> (:compile-time 'require-override)
  ;; Require this to be undefined:
  [a b d] -> (:compile-time :equals nil)
  ;; Require this to be self-insert:
  [a b e] -> (:compile-time :equals #'self-insert-command)
  ;; Require this to be ignored:
  [a b f] -> (:compile-time :equals #'ignore)
  )

(bloodbind! disallow-bindings ()
  "Block arrow key use "
  [ <left> ]  -> (:compile-time :equals #'ignore)
  [ <right> ] -> (:compile-time :equals #'ignore)
  [ <up> ]    -> (:compile-time :equals #'ignore)
  [ <down> ]  -> (:compile-time :equals #'ignore)
  )



;;; compile-time-check.el ends here
