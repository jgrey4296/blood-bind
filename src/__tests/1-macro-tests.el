;; blood--test.el -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;;
;;
(require 'buttercup)
(require 'blood-bind)
(require 'blood-bind--buttercup)

;; binding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(describe "bloodbind general"
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "empty should error when given nothing"
    (expect (macroexpand '(bloodbind! simple ()))
            :to-equal nil)
    )
  (xit "should have an optional docstring"
    (expect (macroexpand '(bloodbind! simple ()
                            [a] :: #'cmd))
            :to-equal '(:name bloodbind-simple :body [a] :: #'cmd)))
  (xit "should ignore comments"
    (expect (macroexpand '(bloodbind! simple ()
                            ;; a comment
                            [a] :: #'cmd))
            :to-equal '(:name bloodbind-simple :body [a] :: #'cmd)))
)

(describe "bloodbind rhs!"
  (it "is a sanity test" (expect t :to-be (not nil)))
  (xit "should handle a single binding"
    (expect (macroexpand '(bloodbind! basic ()
                           [ a ] :: #'cmd
                           ))
            :to-be nil))
  (xit "should handle multiple bindings"
    (expect (macroexpand '(bloodbind! basic ()
                           [ a ] :: #'cmd
                           [ b ] :: #'cmdb
                           ))
            :to-be nil))
  (xit "should handle lambda bindings"
    (expect (macroexpand '(bloodbind! basic ()
                           [ a ] :: #'(lambda () (message "blah"))
                           ))
            :to-be nil))
  (xit "should handle toggle bindings"
    (expect (macroexpand '(bloodbind! basic ()
                           [ a ] :: (:toggle flycheck-mode)
                           ))
            :to-be nil))
  (xit "should handle hook toggle bindings"
        (expect (macroexpand '(bloodbind! basic ()
                                [ a ] :: (:hook prog-mode-hook flycheck-mode)
                           ))
            :to-be nil))
  (xit "should handle on-compile creation"
        (expect (macroexpand '(bloodbind! basic ()
                           [ a ] :: (:on-compile
                           ))
            :to-be nil)))

)
(describe "bloodbind lhs"
  (it "is a sanity test" (expect t :to-be (not nil)))

)
(describe "bloodbind state"
  (it "is a sanity test" (expect t :to-be (not nil)))

)
(describe "bloodbind profile"
  ;; Vars:
  :var (a)
  ;; Setup
  (before-each nil)
  ;; Teardown
  (after-each nil)
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))

)

;; transforms  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(describe "bloodform patterns"
  ;; Vars:
  :var (a)
  ;; Setup
  (before-each nil)
  ;; Teardown
  (after-each nil)
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))

)
(describe "bloodform entry"
  ;; Vars:
  :var (a)
  ;; Setup
  (before-each nil)
  ;; Teardown
  (after-each nil)
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))

)
(describe "bloodform tokens"
  ;; Vars:
  :var (a)
  ;; Setup
  (before-each nil)
  ;; Teardown
  (after-each nil)
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))

)

;; util ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(describe "check process body"
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
  )

