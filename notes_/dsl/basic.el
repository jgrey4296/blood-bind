;;; basic.el -*- lexical-binding: t; no-byte-compile: t; -*-

(bloodbind! basic ()
  [a] :: #'cmd
  )

(bloodbind! with-docstr ()
  "a simple docstr"
  [a] :: #'cmd
  )

(bloodbind! with-lambda ()
  "a lambda promoted to interactive"
  [a] :: #'(lambda () (message "blah"))
  )

(bloodbind! with-toggle ()
  "a variable that will be toggled"
  [a b] :: (:toggle global-flycheck-mode)
 )


;;; basic.el ends here
