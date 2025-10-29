;;; blood-bind--advice.el -*- lexical-binding: t; no-byte-compile: t; -*-
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
;; Local Variables:
;; read-symbol-shorthands: (
;; ("bba-" . "blood-bind--advice-")
;; ("bbc-" . "blood-bind--vars-")
;; )
;; End:
;;; blood-bind--advice.el ends here
