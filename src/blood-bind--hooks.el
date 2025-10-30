;;; blood_bind_hooks.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defvar blood-bind-pre-compile-hook nil "A Hook run before starting to compile keymaps")

(defvar blood-bind-post-compile-hook nil "A hook run after compiling keymaps")

(defun blood-bind-update-keymaps-h ()
  " for change -major-mode-hook.
based on the major-mode, set keymap variables?  "

  )

(provide 'blood-bind--hooks)
;;; blood_bind_hooks.el ends here
