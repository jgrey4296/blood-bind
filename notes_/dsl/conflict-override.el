;;; conflict-override.el -*- lexical-binding: t; no-byte-compile: t; -*-

(bloodbind! confict ()
  [ a b c ] :: #'cmd
  ;; Prohibit overriding:
  [ a b d !! ] :: #'cmd-other
  )

(bloodbind! conflict ()
  "Errors:"
  [ a b c ] :: #'cmd-other
  )

(bloodbind! conflict ()
  "Override the conflict"
  [ !! a b c ] :: #'cmd-other
  )

(bloodbind! conflict ()
  "a failed override, will error"
  [ !! a b d ] :: #'cmd-blah
  )



;;; conflict-override.el ends here
