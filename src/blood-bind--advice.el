;;; blood-bind--advice.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defvar blood-bind--advice-active nil "A toggle to indicate non-blood-bind keybinding has been disabled")

(defvar blood-bind--advice-targets      (list
                                              'keymap-set 'keymap-global-set 'keymap-local-set
                                              'bind-key 'bind-keys* 'evil-define-key 'evil-define-key*
                                              'define-key 'local-set-key 'global-set-key
                                              'local-unset-key 'global-unset-key
                                              )
  "Function symbols to override with advice")

(defun blood-bind--block-keybindings-a (&rest args)
  "Advice to interrupt non-blood-bind keybindings.

Overrides functions listed in `blood-bind--advice-targets'
"

  (signal 'blood-bind-error)
  )

(defun blood-bind-toggle-advice ()
  "Install or remove advice on keybinding functions"
  (if (not blood-bind--advice-active)
      (progn (message "Blood-Bind: Disabling Keybind Functions")
             (cl-loop for sym in blood-bind--advice-targets
                      do
                      (advice-add sym :override #'blood-bind--block-keybindings-a)
                      ))
    (progn (message "Blood-Bind: Re-Activating Keybind Functions")
           (cl-loop for sym in blood-bind--advice-targets
                    do
                    (advice-remove sym #'blood-bind--block-keybindings-a)
                    )))
  (setq blood-bind--advice-active (not blood-bind--advice-active))
  )

(provide 'blood-bind--advice)
;;; blood-bind--advice.el ends here
