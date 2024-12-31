;;; blood-bind--advice.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(defun bba-block-keybindings-a (&rest args)
  "Advice to interrupt non-blood-bind keybindings.

Overrides functions listed in `blood-bind--vars-advice-targets'
"

  (signal 'blood-bind-error)
  )

(defun bba-toggle-advice ()
  "Install or remove advice on keybinding functions"
  (if (not bbv-advice-active)
      (progn (message "Blood-Bind: Disabling Keybind Functions")
             (cl-loop for sym in bbv-advice-targets
                      do
                      (advice-add sym :override #'bba-block-keybindings-a)
                      ))
    (progn (message "Blood-Bind: Re-Activating Keybind Functions")
           (cl-loop for sym in bbv-advice-targets
                    do
                    (advice-remove sym #'bba-block-keybindings-a)
                    )))
  (setq bbv-advice-active (not bbv-advice-active))
  )

(provide 'blood-bind--advice)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    December 31, 2024
;; Modified:   December 31, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("bba-" . "blood-bind--advice-")
;; ("bbc-" . "blood-bind--vars-")
;; )
;; End:
;;; blood-bind--advice.el ends here
