;;; blood_bind_structs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(cl-defstruct bloodbind--store
  " The main store of all bindings to create keymaps from. "
  )

(cl-defstruct bloodbind--entry
  " a single binding entry. a pattern, its result, and metadata "
  )

(cl-defstruct bloodbind--pattern
  " a binding pattern that will be compiled into a keymap "
  )

(cl-defstruct bloodbind--profile
  " a pattern defined collection of keymaps to apply "
  )

(provide 'blood_bind_structs)

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
