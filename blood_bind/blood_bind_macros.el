;;; blood_bind_macros.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;; TODO allow first arg to be docstring
;; TODO record expansion file into generated data

(defmacro bloodbind! (&rest args)
  " declares bindings into the registry using the bloodbind DSL,
    or defines a binding-profile
 "
  )

(defmacro bloodbind-entries-transform! (pattern &rest args)
  " declare a transform that matches on `pattern`,
and calls the code to transform the resulting bindings on compilation "

  )

(defmacro bloodbind-map-transform! (pattern &rest args)
  " delcare a transform applied to a map pattern, post-compile."

  )

(defmacro bloodbind-token-transform! (&rest args) ;; (λ [pattern] -> [pattern] )
  " define how to expand tokens in the pattern dsl
eg:
[a]             -> (kbd \"a\")
[!!]            -> :allow-override
[ :blah! ]      -> blah-mode-map
[ :blah% ]      -> evil-blah-state-map
[ (:state blah) -> evil-blah-state-map
[ :blah? ]      -> ?
"
  )

(provide 'blood_bind_macros)

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
