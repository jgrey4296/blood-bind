;;; global.el -*- lexical-binding: t; no-byte-compile: t; -*-

(bloodbind! global-bindings ()
  "for the global map"
  :global
  [a b c] :: #'cmd

  [ SPC ]   -> :leader
  [ SPC m ] -> :localleader
 )


(bloodbind! global-leaders ()
  :global
  :let [ :leader ] -> 'prefix
  [a b c] :: #'cmd

  :let [ :localleader ] -> 'prefix
  [a b c] :: #'localcmd
  )

;;; global.el ends here
