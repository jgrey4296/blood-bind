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
        (cons t (format "Didn't expect to pass %s, but did" actual-test))
      (cons nil (format "Expected to pass %s, but didn't" actual-test))
        )
    )
  )

(buttercup-define-matcher :slot (expr type slot matcher &rest args)
  "Get the slot from a struct slot of the result of expr "
  (let* ((expr-result (funcall expr))
         (uneval-expr (buttercup--enclosed-expr expr))
         (slot-val (cl-struct-slot-value (funcall type) (funcall slot) expr-result))
         (slotw `(lambda () (quote ,uneval-expr) ,(if (symbolp slot-val)
                                                      `(quote ,slot-val)
                                                    slot-val)))
         (matcher (funcall matcher))
         result
         )
    (funcall #'buttercup--apply-matcher matcher (cons slotw args))
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

(describe "entry"
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

(describe "pattern tests"
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

(describe "transform tests"
  ;; Vars:
  :var (a)
  ;; Setup
  (before-each nil)
  ;; Teardown
  (after-each nil)
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))

)

(describe "collection tests"
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
    (expect (make-blood-bind-collection 'test
                                        ""
                                        ""
                                        (list

                                         )
                                        )
            :to-pass #'blood-bind--collection-p)
    )
)

(describe "collection profile tests"
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))

  )

(describe "compiled tests"
  ;; Vars:
  :var (a)
  ;; Setup
  (before-each nil)
  ;; Teardown
  (after-each nil)
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))

)

(describe "store tests"
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
