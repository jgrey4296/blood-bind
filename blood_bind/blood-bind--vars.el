;;; blood_bind_vars.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(defvar blood-bind--registry nil "where all bindings are stored, of type `blood-bind--store'")

(defvar blood-bind--generated nil "where bindings are assembled into a keymaps")

(defvar blood-bind--profiles nil "patterns that create a collection of binding keymaps")

(defvar blood-bind-pre-compile-hook nil "A Hook run before starting to compile keymaps")

(defvar blood-bind-post-compile-hook nil "A hook run after compiling keymaps")

(defvar blood-bind--vars-delay-symbol 'blood-bind-delay "Symbol that is gates the registration of bindings and transforms")

(defvar blood-bind--vars-advice-active nil "A toggle to indicate non-blood-bind keybinding has been disabled")

(defvar blood-bind--vars-advice-targets      (list
                                              'keymap-set 'keymap-global-set 'keymap-local-set
                                              'bind-key 'bind-keys* 'evil-define-key 'evil-define-key*
                                              'define-key 'local-set-key 'global-set-key
                                              'local-unset-key 'global-unset-key
                                              )
  "Function symbols to override with advice")

(defvar blood-bind--vars-type-kwds           (list :profile ))

(defvar blood-bind--vars-entry-type-kwds     (list :let :endlet))

(defvar blood-bind--vars-lhs-patt-bool-kwds  (list :root :remap :local :implicit-prefix))

(defvar blood-bind--vars-lhs-patt-val-kwds   (list :map :state :remap :local))

(defvar blood-bind--vars-op-kwds             (list :let :bind :submap))

(defvar blood-bind--vars-rhs-kwds            (list :toggle :hook :on-compile :desc :allow-override))

(defvar blood-bind--vars-lhs-symbol-alist   (list '(_ . :implicit-prefix)
                                                  '(\* . :glob)
                                                  '(? . :insert)
                                                  '(!! . :override)
                                                  )
  "A reverse Plist to remap shorthand pattern symbols to their bool keywords"
  )

(defvar blood-bind--vars-op-symbol-alist    (list '(-> . :let) '(:: . :bind) '(=> . :submap))
  "A reverse Plist to remap shorthand operator symbols to their keywords"
  )

(defvar blood-bind--vars-lhs-kwd-suffix-alist (list
                                               '("!" . (:map "-mode-map"))
                                               '("&" . (:map "-minor-mode-map"))
                                               '("^" . (:map "-map"))
                                               '("?" . (:state nil))
                                               )
  "Alist for converting a suffix of a keyword"
  )

(defvar blood-bind--vars-lhs-sep-sym :|:  "Symbol that separates metadata from keybinding")

(define-error 'blood-bind-error "General Blood Bind Error")

(define-error 'blood-bind-parse-error "Parsing an entry failed" 'blood-bind-error)

(define-error 'blood-bind-conflict-error "Two entries conflicted" 'blood-bind-error)

(provide 'blood-bind--vars)

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
;;; blood_bind_vars.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("bbv-" . "blood-bind--vars-")
;; )
;; End:
