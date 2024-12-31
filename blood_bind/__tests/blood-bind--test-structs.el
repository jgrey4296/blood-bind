;; blood--test.el -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;;
;;
(require 'buttercup)

(buttercup-define-matcher :to-signal (expr &optional signal signal-args)
  "Defines :to-signal, which correctly catches cl-assert failures"
  (let ((expected-signal-symbol (and signal (funcall signal)))
        (expected-signal-args (and signal-args (funcall signal-args)))
        (unevaluated-expr (buttercup--enclosed-expr expr))
        (debug-on-error nil)
        expr-value
        thrown-signal)
    ;; Set the above variables
    (condition-case err
        (setq expr-value (funcall expr))
      (cl--assertion-failed
       (setq thrown-signal err)
       nil)
      (error
       (setq thrown-signal err)
       nil)
      )
    (buttercup--handle-to-throw thrown-signal
                                (cons expected-signal-symbol expected-signal-args)
                                unevaluated-expr expr-value)))

(buttercup-define-matcher :to-pass (expr test)
  "Check the expr's return value passes a bool test.
eg: cl-class-p
"
  (let ((actual-test (and test (funcall test))))
    (if (funcall actual-test (funcall expr))
        (cons t (format "Didn't expect to pass %s, but did: %s" actual-test (funcall expr)))
      (cons nil (format "Expected to pass %s, but didn't: %s" actual-test (funcall expr)))
        )
    )
  )

(buttercup-define-matcher :slot (expr type slot matcher &rest args)
  "Get the slot from a struct slot of the result of expr "
  (let* ((expr-result (funcall expr))
         (uneval-expr (buttercup--enclosed-expr expr))
         (slot-val (cl-struct-slot-value (funcall type)
                                         (funcall slot)
                                         expr-result))
         (slotw `(lambda () (quote ,uneval-expr) (quote ,slot-val)))
         (matcher (funcall matcher))
         )
    (buttercup--apply-matcher matcher (cons slotw args))
    )
)

(buttercup-define-matcher :has-length (expr val)
  (let ((ex-val (funcall expr)))
    (cons (and (sequencep ex-val)
               (eq (seq-length ex-val)
                   (funcall val)))
          (format "Expected length %s but got %s" (funcall val) (seq-length ex-val))
          )
    )
  )

(defun build-pattern-get-slot (patt slot)
  (let ((pattern (blood-bind--structs-parse-pattern patt)))
    (pcase slot
      ('nil pattern)
      (x (cl-struct-slot-value 'blood-bind--pattern x pattern))
      )
    )
  )

(defun build-entry-get-slot (slot &rest args)
  (let ((entry (apply #'make-blood-bind-entry args)))
    (pcase slot
      ('pattern (blood-bind--entry-pattern entry))
      ('op (blood-bind--entry-operator entry))
      ('target (blood-bind--entry-target entry))
      ('file (blood-bind--entry-file entry))
      ('meta (blood-bind--entry-meta entry))
      (_ entry)
      )
    )
  )

(describe "entry singular"
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "can build a basic entry"
    (expect (make-blood-bind-entry "" [a b c] ':: #'blah) :to-pass #'blood-bind--entry-p)
    (expect (build-entry-get-slot 'op "" [a b c] ':: #'blah) :to-be :bind)
    (expect (build-entry-get-slot 'op "" [a b c] '=> #'blah) :to-be :submap)
    (expect (build-entry-get-slot 'op "" [a b c] '-> #'blah) :to-be :let)
    (expect (build-entry-get-slot  'target "" [a b c] '=> #'blah) :to-be #'blah)
    (expect (build-entry-get-slot  'meta "" [:python! a b c] ':: #'blah) :to-be nil)
    (expect (build-entry-get-slot  nil "" [:python! :|: a b c] ':: #'blah) :not :to-be nil)
    (expect (build-entry-get-slot  nil "" [:python! :|: _ a b c] ':: #'blah) :not :to-be nil)
    (expect (build-entry-get-slot  nil "" [(:state normal) :|: _ a b c] ':: #'blah) :not :to-be nil)
    (expect (build-entry-get-slot  nil "" [:avar$  a b c] ':: #'blah) :not :to-be nil)
    )
  (it "can build an entry with metadata"
    (expect (build-entry-get-slot 'meta "" [a b c] ':: '(:target #'blah :val t)) :to-equal '(:val t))
    )
  (it "errors if a target cant be found"
    (expect (build-entry-get-slot 'meta "" [a b c] ':: '(:val t)) :to-signal 'cl-assertion-failed)
    )

)

(describe "entry multiple"
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "should parse an empty list"
    (expect (make-blood-bind-entries "source" nil) :to-pass #'plistp)
    (expect (plist-get (make-blood-bind-entries "source" nil) :locals) :to-pass #'null)
    (expect (plist-get (make-blood-bind-entries "source" nil) :entries) :to-pass #'null)
    )
  (it "should parse a non-empty list into a plist"
    (expect (make-blood-bind-entries "source" (list
                                               [a b c] ':: #'blah
                                               ))
            :to-pass #'plistp)
    (expect (plist-get (make-blood-bind-entries "source" (list

                                               [a b c] ':: #'blah
                                               )) :entries)
            :has-length 1)
    (expect (plist-get (make-blood-bind-entries "source" (list
                                                          :let [a b c] '-> #'blah
                                                          :let [d b c] '-> #'bloo
                                                          )) :locals)
            :has-length 2)

    )
  (it "should pass through already existing entries"
    (expect (plist-get (make-blood-bind-entries "source" (list
                                                          (make-blood-bind-entry "" [a b c] ':: #'blah)
                                                          )) :entries)
            :has-length 1)
    )
  (it "should not complain about conflicts"
    (expect (plist-get (make-blood-bind-entries "source" (list
                                                          (make-blood-bind-entry "" [a b c] ':: #'blah)
                                                          (make-blood-bind-entry "" [a b c] ':: #'blah)
                                                          )) :entries)
            :has-length 2)
    )
  (it "should raise errors on bad args"
    (expect (make-blood-bind-entries 'bad-source nil)
            :to-signal 'cl-assertion-failed)
    (expect (make-blood-bind-entries "source" 'bad-data)
            :to-signal 'cl-assertion-failed)
    )
  )

(describe "pattern"
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "can be built with basic values"
    (expect (make--blood-bind-pattern-internal) :not :to-be nil)
    (expect (make--blood-bind-pattern-internal :keys [a b c]) :not :to-be nil)
    (expect (blood-bind--pattern-keys  (make--blood-bind-pattern-internal :keys [a b c])) :to-equal [a b c])
    (expect (make--blood-bind-pattern-internal :keys [a b c]) :slot 'blood-bind--pattern 'keys :to-equal [a b c])
    )
  (it "can be parsed"
    (expect (blood-bind--structs-parse-pattern [a b c]) :to-pass #'blood-bind--pattern-p)
    (expect (blood-bind--structs-parse-pattern [!! a b c]) :not :to-be nil)
    )
  (it "can parse basic metadata"
    (expect (build-pattern-get-slot [#'test] 'meta) :to-equal '(:remap t))
    (expect (build-pattern-get-slot [!! a b c] 'meta) :to-equal '(:override t))
    (expect (build-pattern-get-slot [_ a b c] 'meta) :to-equal '(:implicit-prefix t))
    (expect (build-pattern-get-slot [:python! a b c] 'meta) :to-equal '(:map python-mode-map))
    (expect (build-pattern-get-slot [:python& a b c] 'meta) :to-equal '(:map python-minor-mode-map))
    (expect (build-pattern-get-slot [:python? a b c] 'meta) :to-equal '(:state python))
    (expect (build-pattern-get-slot [(:map python) a b c] 'meta) :to-equal '(:map python))
    (expect (build-pattern-get-slot [:local a b c] 'meta) :to-equal '(:local t))
    )
  (it "sets the keys to the remainder after parsing"
    (expect (build-pattern-get-slot [#'test] 'keys) :to-equal [])
    (expect (build-pattern-get-slot [!! a b c] 'keys) :to-equal [a b c])
    (expect (build-pattern-get-slot [:python! a b c] 'keys) :to-equal [a b c])
    (expect (build-pattern-get-slot [:python& a b c] 'keys) :to-equal [a b c])
    (expect (build-pattern-get-slot [:python& :normal? a b c] 'keys) :to-equal [a b c])
    (expect (build-pattern-get-slot [(:map python) :normal? a b c] 'keys) :to-equal [a b c])
    (expect (build-pattern-get-slot [:local (:map python) :normal? a b c] 'keys) :to-equal [a b c])
    )
  (it "parses up to the separator"
    (expect (build-pattern-get-slot [:python! :normal? :|: a b c] 'keys) :to-equal [a b c])
    )
)

(describe "transform"
  :var (entry)
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "should build an empty transform plist"
    (expect (make-blood-bind-transforms "source" nil) :to-pass #'plistp)
    (expect (plist-get (make-blood-bind-transforms "source" nil) :entriess) :to-be nil)
    )
  (it "should build a non-empty-transform plist"
    (expect (make-blood-bind-transforms "source" (list
                                                  [a b c] '-> [a b c]
                                                  )) :to-pass #'plistp)
    (expect (plist-get (make-blood-bind-transforms "source" (list
                                                  [a b c] '-> [a b c]
                                                  )) :entries)
            :has-length 1)

    (expect (car (plist-get (make-blood-bind-transforms "source" (list
                                                       [a b c] '-> [a b c]
                                                       )) :entries))
            :to-pass #'blood-bind--transform-p)
    )
  (it "should have a pattern and replacement"
    (setq entry (car (plist-get (make-blood-bind-transforms "source" (list
                                                          [a b c] '-> [a b c]
                                                          )) :entries)))
    (expect (blood-bind--transform-pattern entry) :to-pass #'blood-bind--pattern-p)
    (expect (blood-bind--transform-replacement entry) :to-pass #'blood-bind--pattern-p)
    )
)

(describe "collection"
  :var (entries)
  (before-all
    (setq entries (make-blood-bind-entries "source"
                                           (list
                                            (make-blood-bind-entry "" [a b c] ':: #'test)
                                            )))
    )
  (before-each
    (setq blood-bind-global-store (make--blood-bind-store-internal)))
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "can build an empty collection"
    (expect (make-blood-bind-collection 'test "" "" nil)
            :to-pass #'blood-bind--collection-p)
    (expect (make-blood-bind-collection 'test "" "" nil)
            :slot 'blood-bind--collection 'name :to-be 'test)
    (expect (make-blood-bind-collection 'test "blah bloo" "" nil)
            :slot 'blood-bind--collection 'docstr :to-equal "blah bloo")
    (expect (make-blood-bind-collection 'test "blah bloo" "a/file.txt" nil)
            :slot 'blood-bind--collection 'source :to-equal "a/file.txt")
    )
  (it "can build non-empty-collections"
    (expect (make-blood-bind-collection 'test "doc" "source" entries)
                                        :to-pass #'blood-bind--collection-p)
    )
  (it "has entries as a list"
    (expect (make-blood-bind-collection 'test "doc" "source" entries)
            :slot 'blood-bind--collection 'entries :to-pass #'listp)
    )
  (it "signals assertion failures on bad args"
    (expect (make-blood-bind-collection "bad name" nil nil nil) :to-signal 'cl-assertion-failed)
    (expect (make-blood-bind-collection 'name 'bad-doc nil nil) :to-signal 'cl-assertion-failed)
    (expect (make-blood-bind-collection 'name "doc" 'bad-source nil) :to-signal 'cl-assertion-failed)
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
                                        'bad-type
                                        )
            :to-signal 'cl-assertion-failed)
    )
)

(describe "profiles"
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))

  )

(describe "compiled"
  ;; Vars:
  :var (a)
  ;; Setup
  (before-each nil)
  ;; Teardown
  (after-each nil)
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))

)

(describe "store"
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

(describe "meta pattern element parsing"
  (it "it can parse a function"
    (expect (blood-bind--structs-pattern-meta-parse (elt [#'test] 0)) :to-equal '(:remap t))
    )
  (it "can parse an override"
    (expect (blood-bind--structs-pattern-meta-parse (elt [!! a b c] 0)) :to-equal '(:override t))
    )
  (it "can parse a major map"
    (expect (blood-bind--structs-pattern-meta-parse (elt [:python! a b c] 0)) :to-equal '(:map python-mode-map))
    )
  (it "can parse a minor map"
    (expect (blood-bind--structs-pattern-meta-parse (elt [:python& a b c] 0)) :to-equal '(:map python-minor-mode-map))
    )
  (it "can parse an explicit map"
    (expect (blood-bind--structs-pattern-meta-parse (elt [:python-blah^ a b c] 0)) :to-equal '(:map python-blah-map))
    )
  (it "can parse a state"
    (expect (blood-bind--structs-pattern-meta-parse (elt [:insert? a b c] 0)) :to-equal '(:state insert))
    )
  (it "can parse explicit kwd pairs"
    (expect (blood-bind--structs-pattern-meta-parse (elt [(:state normal) a b c] 0)) :to-equal '(:state normal))
    )
  (it "can parse the pattern separator"
    (expect (blood-bind--structs-pattern-meta-parse (elt [:|: a b c] 0)) :to-equal t)
    )
  (it "can parse kwd wildcards"
    (expect (blood-bind--structs-pattern-meta-parse (elt [:python*! a b c] 0)) :to-equal '(:map python*-mode-map))
    (expect (blood-bind--structs-pattern-meta-parse (elt [:python?! a b c] 0)) :to-equal '(:map python?-mode-map))
    )
  (it "returns nil otherwise"
    (expect (blood-bind--structs-pattern-meta-parse (elt [:myvar$ a b c] 0)) :to-equal nil)
    )
  )

(describe "op parsing"
  (it "should recognise basic binding"
    (expect (blood-bind--structs-parse-op '::) :to-be :bind)
    )
  (it "should recognise let binding"
    (expect (blood-bind--structs-parse-op '->) :to-be :let)
    )
  (it "should recognise submap binding"
    (expect (blood-bind--structs-parse-op '=>) :to-be :submap)
    )
  (it "should return nil otherwise"
    (expect (blood-bind--structs-parse-op 'blah) :to-be nil)
    )

  )
