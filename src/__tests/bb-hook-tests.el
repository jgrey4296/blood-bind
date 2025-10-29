;;; bb-hook-tests.el -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'buttercup)
(require 'blood-bind)

(describe "sanity"
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
)
