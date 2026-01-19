;;; modes.el -*- lexical-binding: t; no-byte-compile: t; -*-

(bloodbind! simple-modes ()
  "simple binding to a mode's map

:{}! expands to {}-mode-map
"
  [:python! a b] :: #'cmd
  )

(bloodbind! mode-prefix ()
  "setting a mode as part of the prefix"
  :let [ :python! ] -> 'prefix
  [a b] :: #'cmd
  )

(bloodbind! minor-prefix ()
  "binding for a minor mode

:{}& expands to {}-minor-mode-map
"
  :let [ :yas& ] -> 'prefix
  [a b] -> #'cmd
  )

;;; modes.el ends here
