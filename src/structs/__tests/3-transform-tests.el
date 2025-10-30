;;; bb-transform-tests.el -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'buttercup)
(require 'blood-bind-structs)
(require 'blood-bind--buttercup)

(describe "transform: basic"
  :var (entry)
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "should build an empty transform plist"
    (setq entry (make-blood-bind-transforms "source" nil))
    (expect entry :to-pass #'plistp)
    (expect entry
            :plist-get :entries
            :to-be nil)
    )
  (it "should build a non-empty-transform plist"
    (setq entry (make-blood-bind-transforms "source" (list [a b c] '-> [d e f g])))
    (expect entry :to-pass #'plistp)
    (expect entry :plist-get :entries :has-length 1)
    (expect entry :plist-get :entries :car :to-pass #'blood-bind--transform-p)
    )
  (it "should have a pattern"
    (setq entry (make-blood-bind-transforms "source" (list [a b c] '-> [d e f g])))
    (expect entry
            :plist-get :entries
            :car
            :slot 'blood-bind--transform 'pattern
            :to-pass #'blood-bind--pattern-p)
    (expect entry
            :plist-get :entries
            :car
            :slot 'blood-bind--transform 'pattern
            :slot 'blood-bind--pattern 'keys
            :to-equal [a b c])
    )
  (it "should have a replacement"
    (setq entry (make-blood-bind-transforms "source" (list [a b c] '-> [d e f g])))
    (expect entry
            :plist-get :entries
            :car
            :slot 'blood-bind--transform 'replacement
            :to-pass #'blood-bind--pattern-p)
    (expect entry
            :plist-get :entries
            :car
            :slot 'blood-bind--transform 'replacement
            :slot 'blood-bind--pattern 'keys
            :to-equal [d e f g])
    )
)
