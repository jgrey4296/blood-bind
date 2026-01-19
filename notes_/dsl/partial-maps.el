;;; imports.el -*- lexical-binding: t; no-byte-compile: t; -*-

(bloodbind! basic-collections ()
  "Named collections in the profile"
  :let [ :blah^ ] -> 'prefix
  [a b c] :: #'cmd

  :let [ :bloo^ ] -> 'prefix
  [a b c] :: #'cmd-other

  ;; continue with blah:
  :let [ :blah^ ] -> 'prefix
  [a b d] :: #'cmd-other

  )

(bloodbind! parents ()
  "Defined a parent -> child relationship

Errors at compile time if parent or child aren't defined.
"
  :parent ( :blah^ :bloo^ )

  :let [ :blah^ ] -> 'prefix
  [a b c] :: #'cmd

  :let [ :bloo^ ] -> 'prefix
  [a b d] :: #'cmd-other
  )

(bloodbind! basic-merging ()
  :let [ :root^ ] -> 'prefix
  [a b c] :: #'cmd
  [a b d] :: #'blah

  ;; Overrides root's blah:
  :let [ :merge-under^ ] -> 'prefix
  [ ++ :root^ ++ ] =>
  [a b d] :: #'bloo

  ;; Does't override root's blah:
  :let [ :merge-under^ ] -> 'prefix
  [a b d] :: #'bloo
  [ ++ root^ ++ ] =>
  )

(bloodbind! selective-merging ()
  :let [ :root^ ] -> 'prefix
  [:python! a b c] :: #'cmd
  [:elixir! a b d] :: #'blah

  ;; only merge the python bindings:
  :let [ :python! ] -> 'prefix
  [ ++ :root^ :python! ++ ] =>
  [a b d] :: #'bloo
  )

(bloodbind! multi-merge ()
  :let [ :first^ ] -> 'prefix
  [a b c] :: #'blah

  :let [ :second^ ] -> 'prefix
  [a b d] :: #'bloo

  :let [ ] -> 'prefix
  [ ++ :first^ ++ ] =>
  [ ++ :second^ ++ ] =>
  ;; merge from different profile
  [ ++ (:profile basic-merging) :root^ ++ ] =>
  [a b e] :: #'aweg
  )

(bloodbind! prefix-bind ()
  "bind a collection to a prefix in the current collection:

blah-map doesn't exist in the global namespace, only inside bloodbind
"
  :let [ :blah^ ] -> 'prefix
  [a b c] :: #'cmd

  :let [ ] -> 'prefix
  ;; bind 'd' to blah-map
  [ d ] => [ :blah^ ]
  )

;;; imports.el ends here
