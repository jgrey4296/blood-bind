;;; blood_bind_structs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(cl-defstruct blood-bind--store
  " The main store of all profiles, pre- and post- compilation "
  (profiles (make-hash-table)  :type 'hash-table)
  (compiled (make-hash-table)  :type 'hash-table)
  )

(cl-defstruct blood-bind--profile
  " a named collection of entries.
at compile: all(entries) -> (list major-list minor-list state-list )
then foreach sym: 'sym-profile-name-map
(eg: 'python -> python-profile-default-map)
on profile (apply default): (setq python-mode-map python-profile-default-map)

"
  (name nil    :type 'symbol)
  (globals nil :type 'list) ;; all global profile bindings
  (entries nil :type 'list) ;; list[ ([local-bindings], [entries]) ]
  )

(cl-defstruct blood-bind--compiled-profile
  " The compiled profile, with local maps
when applied, loops (setq maps.key maps.value)
"
  (name nil :type 'symbol)
  (maps (make-hash-table) :type 'hash-table)
  )

(cl-defstruct blood-bind--entry
  " a single binding entry. a pattern, its target, and metadata.
can be:
([pattern] #'cmd (meta)) : standard binding
([pattern]  :kw (:bind 'global)) : global binding
([pattern]  :kw (:bind 'local))  : local binding

 "
  (pattern nil    :type 'blood-bind--pattern)
  (target nil     :type 'symbol-or-kw)
  (meta nil       :type 'plist)
  (file nil       :type 'str)
  (expanded nil   :type 'bool)
  )

(cl-defstruct blood-bind--pattern
  " a binding pattern that will be compiled into a keymap
eg: ('python-mode-map 'normal
"
  (map nil   :type 'list)
  (state nil :type 'symbol)
  (keys nil  :type 'list)
  )

(provide 'blood-bind-structs)

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
;;; blood_bind_structs.el ends here
