;;; let-binding.el -*- lexical-binding: t; no-byte-compile: t; -*-

(bloodbind! let-val ()
  "binds [a b c] to cmd"
  :let [a b] -> 'var
  [ :var c ] :: #'cmd
  )

(bloodbind! implicit-head ()
  "Binds [a b c] to cmd"
  :let [a b] -> 'prefix
  [c] :: #'cmd
  )

(bloodbind! pattern-reuse ()
  "define a local pattern variable for this block only"
  :let [a b c] -> 'ex

  [ :ex d ] :: #'cmd
  [ d :ex ] :: #'cmd
  )

(bloodbind! profile-pattern ()
  "define a pattern for use in this profile"
  [ a b c ] -> 'var
  )

(bloodbind! profile-pattern ()
  "use the profile pattern"
  [ a b :var ] :: #'cmd
  )


(bloodbind! alternatives ()
  "define alternatives to multi binding,

so both [python a b] and [elixir a b] are bound to cmd
"
  :let [ < :python! :elixir! > ] -> 'prefix
  [a b] :: #'cmd
  )

;;; let-binding.el ends here
