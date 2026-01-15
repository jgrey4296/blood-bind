;;; complex.el -*- lexical-binding: t; no-byte-compile: t; -*-

(bloodbind! complex-mode-state ()
  "mixing mode+state combination"
  [ :python! :visual? a b] :: #'cmd
  )

(bloodbind! submap-binding ()
  [ a b ] => 'blah-map
  ;; sugar:
  [ a b ] => :blah^
  )

(bloodbind! wildcards ()
  ;; For all major-mode, bind [a b] -> cmd
  [ :*! a b ] :: #'cmd
  ;; For all minor-modes
  [ :*& a b ] :: #'cmd
  ;; For all states
  [ :*? a b ] :: #'cmd
  )

(bloodbind! ranges ()
  ;; key ranges:
  [ a b <j-l> ] :: #'cmd
  ;; state ranges:
  [ < :normal? :visual? > a b ] :: #'cmd
  )


;;; complex.el ends here
