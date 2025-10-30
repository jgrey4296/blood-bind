;;; bb-pattern-tests.el -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'buttercup)
(require 'blood-bind-structs)
(require 'blood-bind--buttercup)

(buttercup-define-matcher :bb-parse-pattern (expr matcher &rest args)
  "Utility to parse a blood bind pattern"
  (let* ((expr-result (funcall expr))
         (uneval-expr (buttercup--enclosed-expr expr))
         (val (blood-bind--parse-pattern expr-result))
         (valw `(lambda () (quote ,uneval-expr) (quote ,val)))
         (matcher (funcall matcher))
         )
    (buttercup--apply-matcher matcher (cons valw args))
    )
  )

(buttercup-define-matcher :bb-parse-meta (expr matcher &rest args)
  "Utility to parse a blood bind pattern"
  (let* ((expr-result (funcall expr))
         (uneval-expr (buttercup--enclosed-expr expr))
         (val (blood-bind--parse-meta expr-result))
         (valw `(lambda () (quote ,uneval-expr) (quote ,val)))
         (matcher (funcall matcher))
         )
    (buttercup--apply-matcher matcher (cons valw args))
    )
  )

(describe "pattern: meta parsing"
  (it "it can parse a function remap"
    (expect (elt [#'test] 0)
            :bb-parse-meta
            :to-equal '(:remap t))
    )
  (it "can parse an override"
    (expect (elt [!! a b c] 0)
            :bb-parse-meta
            :to-equal '(:override t))
    )
  (it "can parse a major map"
    (expect (elt [:python! a b c] 0)
            :bb-parse-meta
            :to-equal '(:map python-mode-map))
    )
  (it "can parse a minor map"
    (expect (elt [:python& a b c] 0)
            :bb-parse-meta
            :to-equal '(:map python-minor-mode-map))
    )
  (it "can parse an explicit map"
    (expect (elt [:python-blah^ a b c] 0)
            :bb-parse-meta
            :to-equal '(:map python-blah-map))
    )
  (it "can parse a state"
    (expect (elt [:insert? a b c] 0)
            :bb-parse-meta
            :to-equal '(:state insert))
    )
  (it "can parse explicit kwd pairs"
    (expect (elt [(:state normal) a b c] 0)
            :bb-parse-meta
            :to-equal '(:state normal))
    )
  (it "can parse the pattern separator"
    (expect (elt [:|: a b c] 0)
            :bb-parse-meta
            :to-equal t)
    )
  (it "can parse kwd wildcards"
    (expect (elt [:python*! a b c] 0)
            :bb-parse-meta
            :to-equal '(:map python*-mode-map))
    (expect (elt [:python?! a b c] 0)
            :bb-parse-meta
            :to-equal '(:map python?-mode-map))
    )
  (it "returns nil otherwise"
    (expect (elt [:myvar$ a b c] 0)
            :bb-parse-meta
            :to-equal nil)
    )
  )

(describe "pattern: basic"
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "can be built with basic values"
    (expect (make-blood-bind--pattern) :not :to-be nil)
    (expect (make-blood-bind--pattern :keys [a b c])
     :not :to-be nil)
    (expect (make-blood-bind--pattern :keys [a b c])
            :slot 'blood-bind--pattern 'keys
            :to-equal [a b c])
    )
)

(describe "pattern: parsing"
  (it "can be parsed"
    (expect [a b c]
            :bb-parse-pattern
            :to-pass #'blood-bind--pattern-p)
    (expect [!! a b c]
            :bb-parse-pattern
            :not :to-be nil)
    )
  (it "can parse basic metadata"
    (expect [#'test]
            :bb-parse-pattern
            :slot 'blood-bind--pattern 'meta
            :to-equal '(:remap t))
    (expect [!! a b c]
            :bb-parse-pattern
            :slot 'blood-bind--pattern 'meta
            :to-equal '(:override t))
    (expect [_ a b c]
            :bb-parse-pattern
            :slot 'blood-bind--pattern 'meta
            :to-equal '(:implicit-prefix t))
    (expect [:python! a b c]
            :bb-parse-pattern
            :slot 'blood-bind--pattern 'meta
            :to-equal '(:map python-mode-map))
    (expect [:python& a b c]
            :bb-parse-pattern
            :slot 'blood-bind--pattern 'meta
            :to-equal '(:map python-minor-mode-map))
    (expect [:python? a b c]
            :bb-parse-pattern
            :slot 'blood-bind--pattern 'meta
            :to-equal '(:state python))
    (expect [(:map python) a b c]
            :bb-parse-pattern
            :slot 'blood-bind--pattern 'meta
            :to-equal '(:map python))
    (expect [:local a b c]
            :bb-parse-pattern
            :slot 'blood-bind--pattern 'meta
            :to-equal '(:local t))
    )
  (it "can parse key sequences"
    (expect [#'test]
            :bb-parse-pattern
            :slot 'blood-bind--pattern 'keys
            :to-equal [])
    (expect [!! a b c]
            :bb-parse-pattern
            :slot 'blood-bind--pattern 'keys
            :to-equal [a b c])
    (expect [:python! a b c]
            :bb-parse-pattern
            :slot 'blood-bind--pattern 'keys
            :to-equal [a b c])
    (expect [:python& a b c]
            :bb-parse-pattern
            :slot 'blood-bind--pattern 'keys
            :to-equal [a b c])
    (expect [:python& :normal? a b c]
            :bb-parse-pattern
            :slot 'blood-bind--pattern 'keys
            :to-equal [a b c])
    (expect [(:map python) :normal? a b c]
            :bb-parse-pattern
            :slot 'blood-bind--pattern 'keys
            :to-equal [a b c])
    (expect [:local (:map python) :normal? a b c]
            :bb-parse-pattern
            :slot 'blood-bind--pattern 'keys
            :to-equal [a b c])
    )
  (it "parses up to the separator"
    (expect [:python! :normal? :|: a b c]
            :bb-parse-pattern
            :slot 'blood-bind--pattern 'keys
            :to-equal [a b c])
    )
  )

