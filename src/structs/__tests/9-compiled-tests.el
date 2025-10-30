;; blood--test.el -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;;
;;
(require 'buttercup)
(require 'blood-bind-structs)
(require 'blood-bind--buttercup)

(describe "compiled: basic keybinding from entry"
  :var (entry entry2 conflict-entry)
  (before-all
    (cl-destructuring-bind (e1 e2 e3)
        (plist-get
         (make-blood-bind-entries "source"
                                  (list
                                   [a b c] ':: #'test
                                   [d e f] ':: #'blah
                                   [a b c] ':: #'bloo
                                   ))
         :entries)
      (setq entry  e1
            entry2 e2
            conflict-entry e3
            blood-bind--registry (make-blood-bind--store)
          )
      ))
  (before-each
    (clrhash (blood-bind--store-partial-maps blood-bind--registry)))
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "errors when given bad args"
    (expect (blood-bind--compile-add-entry-to-partial "bad map" entry)
            :to-signal 'cl-assertion-failed)
    (expect (blood-bind--compile-add-entry-to-partial 'mapname 'badentry)
            :to-signal 'cl-assertion-failed)
    )
  (it "errors if keybind fns have been advised"
    (let ((blood-bind--advice-active t))
      (expect (blood-bind--compile-add-entry-to-partial 'mapname entry)
              :to-signal 'cl-assertion-failed))
    )
  (it "binds when given a literal keymap"
    (let ((result (blood-bind--compile-add-entry-to-partial (make-sparse-keymap) entry)))
      (expect result :to-pass #'keymapp)
      (expect (keymap-lookup result "a b c")
              :to-equal #'test)
      )
    )
  (it "errors on a conflict"
    (let ((result (blood-bind--compile-add-entry-to-partial (make-sparse-keymap) entry)))
      (expect (blood-bind--compile-add-entry-to-partial result conflict-entry)
              :to-signal 'blood-bind-conflict-error)
      )
    )

)

(describe "compiled: local bindings are used"
  ;; Vars:
  :var (a)
  ;; Setup
  (before-each nil)
  ;; Teardown
  (after-each nil)
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))

)

(describe "compiled: global bindings are used"
  ;; Vars:
  :var (a)
  ;; Setup
  (before-each nil)
  ;; Teardown
  (after-each nil)
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))

)

(describe "compiled: collections build multiple partial maps"
  ;; Vars:
  :var (a)
  ;; Setup
  (before-each nil)
  ;; Teardown
  (after-each nil)
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))

)

(describe "compiled: combining partial maps"
  (it "is a sanity test" (expect t :to-be (not nil)))

)

(describe "compiled: get map names"
  ;; Vars:
  :var (a)
  ;; Setup
  (before-each nil)
  ;; Teardown
  (after-each nil)
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))

)


