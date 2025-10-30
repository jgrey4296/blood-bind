;;; bb-collection-tests.el -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'buttercup)
(require 'blood-bind-structs)
(require 'blood-bind--buttercup)

(describe "collection: basic"
  :var (entries)
  (before-all
    (setq entries (make-blood-bind-entries "source"
                                           (list
                                            (make-blood-bind-entry "" [a b c] ':: #'test)
                                            )))
    )
  (before-each
    (setq blood-bind--registry (make-blood-bind--store)))
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "can build an empty collection"
    (expect (make-blood-bind-collection 'test "" "" nil)
            :to-pass #'blood-bind--collection-p)
    (expect (make-blood-bind-collection 'test "" "" nil)
            :slot 'blood-bind--collection 'name
            :to-be 'test)
    (expect (make-blood-bind-collection 'test "blah bloo" "" nil)
            :slot 'blood-bind--collection 'docstr
            :to-equal "blah bloo")
    (expect (make-blood-bind-collection 'test "blah bloo" "a/file.txt" nil)
            :slot 'blood-bind--collection 'source
            :to-equal "a/file.txt")
    )
  (it "can build non-empty-collections"
    (expect (make-blood-bind-collection 'test "doc" "source" entries)
            :to-pass #'blood-bind--collection-p)
    )
  (it "has entries as a list"
    (expect (make-blood-bind-collection 'test "doc" "source" entries)
            :slot 'blood-bind--collection 'entries
            :to-pass #'listp)
    )
  (it "signals assertion failures on bad args"
    (expect (make-blood-bind-collection "bad name" nil nil nil)
            :to-signal 'cl-assertion-failed)
    (expect (make-blood-bind-collection 'name 'bad-doc nil nil)
            :to-signal 'cl-assertion-failed)
    (expect (make-blood-bind-collection 'name "doc" 'bad-source nil)
            :to-signal 'cl-assertion-failed)
    (expect (make-blood-bind-collection 'name "doc" "source"
                                        (list 'bad-entries))
            :to-signal 'cl-assertion-failed)
    (expect (make-blood-bind-collection 'name "doc" "source"
                                        (list :entries nil :locals nil)
                                        (list "bad-globals"))
            :to-signal 'cl-assertion-failed)
    (expect (make-blood-bind-collection 'name "doc" "source"
                                        (list :entries nil :locals nil)
                                        (list 'global1 'global2)
                                        'bad-type)
            :to-signal 'cl-assertion-failed)
    )
)
