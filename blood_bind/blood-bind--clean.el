;;; key-clean.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;; key-clean.el, 2024-01-29.
;; Utilities to strip out annoying bindings from all keymaps such as:
;; C-{}, M-{}, C-M-{}, mouse, menu-bar, F1-F9...
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(defun blood-bind--clean-undefine-metas (the-map)
  " For a keymap, remove all bindings of the form:
  C-[a-z] C-M-[a-z] M-[a-z]
  # TODO: undefine pinch,
  "
  (cl-assert (keymapp the-map))
  (cl-loop for acc-map in (accessible-keymaps the-map)
        do
        (if (not (keymapp acc-map))
            (setq acc-map (cdr acc-map)))
        (cl-loop for x in (number-sequence ?a ?z)
                do
                (let ((fmt (char-to-string x)))
                  (if (lookup-key acc-map (kbd (format "C-%s" fmt)))
                      (define-key acc-map (kbd (format "C-%s" fmt)) nil)
                    )
                  (if (and (keymapp acc-map)
                           (lookup-key acc-map (kbd (format "C-M-%s" fmt))))
                      (define-key acc-map (kbd (format "C-M-%s" fmt)) nil)
                    )
                  (if (lookup-key acc-map (kbd (format "M-%s" fmt)))
                      (define-key acc-map (kbd (format "M-%s" fmt)) nil)
                    )
                  )
                )
        (mapc (lambda (x) (if (and (proper-list-p x) (>= 1 (length x)))
                              (delete x acc-map))) acc-map)
        )
  )

(defun blood-bind--clean-undefine-mouse (kmap)
  " TODO For a keymap, remove all bindings using the mouse:
  [mouse-1] [mouse-2] [mouse-3] [mouse-4]
  S-mouse, wheel-down/up, drag-mouse, down-mouse,
  "
  (cl-assert (keymapp kmap))
  (cl-loop for acc-map in (accessible-keymaps kmap)
           do
           (if (not (keymapp acc-map))
               (setq acc-map (cdr acc-map)))
           (if (lookup-key acc-map '[mouse-1])
               (define-key acc-map '[mouse-1] nil))
           (if (lookup-key acc-map '[mouse-2])
               (define-key acc-map '[mouse-2] nil))
           (if (lookup-key acc-map '[mouse-3])
               (define-key acc-map '[mouse-3] nil))
           (if (lookup-key acc-map '[mouse-4])
               (define-key acc-map '[mouse-4] nil))
           )

           (mapc (lambda (x) (if (and (proper-list-p x) (>= 1 (length x)))
                                 (delete x acc-map))) acc-map)
  )

(defun blood-bind--clean-undefine-menu-bar (kmap)
  " TODO remove menu/tab/tool-bar bindings "
  (cl-assert (keymapp kmap))
  (cl-loop for acc-map in (accessible-keymaps kmap)
           do
           (if (not (keymapp acc-map))
               (setq acc-map (cdr acc-map)))
           (if (lookup-key acc-map '[menu-bar])
               (define-key acc-map '[menu-bar] nil))

           (mapc (lambda (x) (if (and (proper-list-p x) (>= 1 (length x)))
                                 (delete x acc-map))) acc-map)
           )
  )

(defun blood-bind--clean-undefine-f-keys (kmap)
  " TODO remove f1-f9 bindings "
  (cl-assert (keymapp kmap))
  (cl-loop for acc-map in (accessible-keymaps kmap)
           do
           (if (not (keymapp acc-map))
               (setq acc-map (cdr acc-map)))
           (if (lookup-key acc-map '[menu-bar])
               (define-key acc-map '[menu-bar] nil))

           (mapc (lambda (x) (if (and (proper-list-p x) (>= 1 (length x)))
                                 (delete x acc-map))) acc-map)
           )
  )

(provide 'blood-bind--clean)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    January 29, 2024
;; Modified:   January 29, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; key-clean.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("bbc-" . "blood-bind--clean-")
;; )
;; End:
