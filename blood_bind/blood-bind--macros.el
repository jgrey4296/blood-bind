;;; blood_bind_macros.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;; TODO first arg as docstring
;; TODO record expansion file into generated data

(defmacro bloodbind! (&rest args)
  " adds entries into the a profile in the registry
    using the bloodbind DSL.
Doesn't check for conflicts, or do expansions.

DSL:
[a b c] -> :kw                         ;; = bind pattern to kw
let [a b c] -> :kw                     ;; = local bind pattern
[a b c] :: #'cmd                       ;; = pattern entry
[a \"b\" c] :: #'cmd                   ;; = pattern enty
[a b c] :: #'cmd (:kw val :kw val)     ;; = metadata plist
[a b c] :: #'(lambda () (...))         ;; = autowrap in interactive
[a b c] :: (:toggle 'mode)             ;; = toggle minor modes on/off
[ #'x #'y ] :: #'cmd                   ;; = any pattern that targets x or y uses cmd instead
[:kw ] ...                             ;; = expand kw to pattern ([a b c])
[:kw!]                                 ;; = 'kw-mode-map
[:kw&]                                 ;; = 'kw-minor-mode-map
[:kw?]                                 ;; = kw-state
[ (:map python-mode-map) ]             ;; = explicit map. equiv to :python!
[ (:state name) ]                      ;; = explicit state. equiv to :name?
[    ]                                 ;; =

(start :kw!)                           ;; = all follow implicitly have :kw!/:kw&/:kw?
(start :kw! :kw& :kw?)                 ;; = composite
(start (:map python-mode-map))         ;; = explicit map naming

 "
  )

(defmacro blood-bind-pattern-transform! (pattern &rest result)
  " declare a transform that matches on `pattern`,
and calls the code to transform the resulting bindings on compilation "

  )

(defmacro blood-bind-entry-transform! (entry-p &rest result)
  "declare a transform for an entry-p lambda into result"
  )

(defmacro blood-bind-token-transform! (&rest args) ;; (λ [pattern] -> [pattern] )
  " LATER define how to expand tokens in the pattern dsl
eg:
[a]             -> (kbd \"a\")
[!!]            -> :allow-override
[ :blah! ]      -> blah-mode-map
[ :blah? ]      -> evil-blah-state-map
[ (:state blah) -> evil-blah-state-map
[ :blah? ]      -> ?
"
  )

(provide 'blood-bind--macros)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    April 03, 2024
;; Modified:   April 03, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; blood_bind_macros.el ends here
