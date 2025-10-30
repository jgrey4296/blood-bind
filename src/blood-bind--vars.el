;;; blood_bind_vars.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defvar blood-bind--generated nil "where bindings are assembled into a keymaps")

(defvar blood-bind--delay-symbol 'blood-bind-delay "Symbol that is gates the registration of bindings and transforms")

(define-error 'blood-bind-error "General Blood Bind Error")

(define-error 'blood-bind-parse-error "Parsing an entry failed" 'blood-bind-error)

(define-error 'blood-bind-conflict-error "Two entries conflicted" 'blood-bind-error)

(provide 'blood-bind--vars)
;;; blood_bind_vars.el ends here
