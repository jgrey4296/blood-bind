;; blood--test.el -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; General ERT tests for blood
;;
(require 'f)

(defun blood-bind--test-setup-core ()
  (add-to-list 'load-path (f-parent default-directory))
  (load "blood-bind")
  )

(ert-deftest blood-bind-test-simple ()
  "Tests "
  (should nil)
)
