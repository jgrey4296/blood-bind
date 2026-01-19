;;; states.el -*- lexical-binding: t; no-byte-compile: t; -*-

(bloodbind! basic-state ()
  "binding for an evil state

:{}? -> evil-{}-state-map
"
  [ :normal? a b ] :: #'cmd
  )

(bloodbind! state-var ()
  :let [ :normal? ] -> :n?
  [ :n? a b ] :: #'cmd
  )


;;; states.el ends here
