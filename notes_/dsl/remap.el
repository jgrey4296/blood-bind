;;; remap.el -*- lexical-binding: t; no-byte-compile: t; -*-

(bloodbind! remap-example ()
  [ a b c ] :: #'cmd
  [ :remap #'cmd ] -> #'cmd-other
  [ :remap a b c ] -> [a b d]
  )


;;; remap.el ends here
