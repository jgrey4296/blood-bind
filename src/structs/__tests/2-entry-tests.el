;;; bb-entry-tests.el -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'buttercup)
(require 'blood-bind-structs)
(require 'blood-bind--buttercup)

(describe "entry: op parsing"
  (it "should recognise basic binding"
    (expect (blood-bind--entries-parse-op '::) :to-be :bind)
    )
  (it "should recognise let binding"
    (expect (blood-bind--entries-parse-op '->) :to-be :let)
    )
  (it "should recognise submap binding"
    (expect (blood-bind--entries-parse-op '=>) :to-be :submap)
    )
  (it "should return nil otherwise"
    (expect (blood-bind--entries-parse-op 'blah) :to-be nil)
    )

  )

(describe "entry: basic"
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "can build a basic entry"
    (expect (make-blood-bind-entry "" [a b c] ':: #'blah)
            :to-pass #'blood-bind--entry-p))
  (it "can bind a function"
    (expect (make-blood-bind-entry "" [a b c] ':: #'blah)
            :slot 'blood-bind--entry 'operator
            :to-be :bind))
  (it "can bind a submap"
    (expect (make-blood-bind-entry "" [a b c] '=> #'blah)
            :slot 'blood-bind--entry 'operator
            :to-be :submap)
    (expect (make-blood-bind-entry  "" [a b c] '=> #'blah)
            :slot 'blood-bind--entry 'target
            :to-be #'blah)
    )
  (it "can bind a let sequence"
    (expect (make-blood-bind-entry "" [a b c] '-> #'blah)
            :slot 'blood-bind--entry 'operator
            :to-be :let))
  (it "can bind to a major mode"
    (expect (make-blood-bind-entry  "" [:python! a b c] ':: #'blah)
            :slot 'blood-bind--entry 'meta
            :to-be nil)
    (expect (make-blood-bind-entry  "" [:python! :|: a b c] ':: #'blah)
            :not :to-be nil)
    (expect (make-blood-bind-entry  "" [:python! :|: _ a b c] ':: #'blah)
            :not :to-be nil)
    )
  (it "can bind to a state"
    (expect (make-blood-bind-entry  "" [(:state normal) :|: _ a b c] ':: #'blah)
            :not :to-be nil)
    )
  (it "can bind a key sequence variable"
    (expect (make-blood-bind-entry  "" [:avar$  a b c] ':: #'blah)
            :not :to-be nil)
    )
  (it "can build an entry with metadata"
    (expect (make-blood-bind-entry "" [a b c] ':: '(:target #'blah :val t))
            :slot 'blood-bind--entry 'meta
            :to-equal '(:val t))
    )
  (it "errors if a target cant be found"
    (expect (make-blood-bind-entry "" [a b c] ':: '(:val t))
            :to-signal 'cl-assertion-failed)
    )
  (it "records the source of the entry"
    (expect (make-blood-bind-entry "a/made/up/file.el" [a b c] ':: '(:target #'blah :val t))
            :slot 'blood-bind--entry 'source
            :to-equal "a/made/up/file.el")
    )
)

(describe "entry: multiple"
  :var (entry)
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "should parse an empty list"
    (setq entry (make-blood-bind-entries "source" nil))
    (expect entry :to-pass #'plistp)
    (expect entry :plist-get :locals :to-pass #'null)
    (expect entry :plist-get :entries :to-pass #'null)
    )
  (it "should parse a non-empty list into a plist"
    (setq entry (make-blood-bind-entries
                 "source" (list [a b c] ':: #'blah)))
    (expect entry :to-pass #'plistp)
    (expect entry :plist-get :entries :has-length 1)
    )
  (it "should parse local let bindings"
    (expect (make-blood-bind-entries "source" (list
                                               :let [a b c] '-> #'blah
                                               :let [d b c] '-> #'bloo
                                               ))
            :plist-get :locals
            :has-length 2)
    )
  (it "should pass through already existing entries"
    (setq entry (make-blood-bind-entries
                 "source" (list (make-blood-bind-entry "" [a b c] ':: #'blah))))
    (expect entry :plist-get :entries :has-length 1)
    )
  (it "should not complain about conflicts"
    (setq entry
          (make-blood-bind-entries "source" (list
                                             (make-blood-bind-entry "" [a b c] ':: #'blah)
                                             (make-blood-bind-entry "" [a b c] ':: #'blah)
                                             )))
    (expect entry :plist-get :entries :has-length 2)
    )
  (it "should raise errors on bad args"
    (expect (make-blood-bind-entries 'bad-source nil)
            :to-signal 'cl-assertion-failed)
    (expect (make-blood-bind-entries "source" 'bad-data)
            :to-signal 'cl-assertion-failed)
    )
  )

