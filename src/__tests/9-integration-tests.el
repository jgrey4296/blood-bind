;; blood--test.el -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;;
;;
(require 'buttercup)
(require 'blood-bind)
(require 'blood-bind--buttercup)

(describe "general bloodbind"
  (it "is a sanity test" (expect t :to-be (not nil)))

)
