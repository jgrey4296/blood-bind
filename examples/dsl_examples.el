;;; dsl_examples.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(bloodbind! basic ()
  "docstring of basic examples"
  ;; General form: [pattern] :: #'cmd (:metadata)*
  ;; global, stateless bindings:
  [a] :: #'cmd
  ;; auto wrap lambdas with (interactive)
  [b] :: #'(lambda () (message "blah"))

  ;; auto mode toggles
  [ t q ] :: (:toggle global-flycheck-mode)
  ;; call function on compile for bind cmd/target
  [ t e ] :: (:on-compile (transient-make-call! evil-embrace "E"))
  ;; major mode map binding:
  [ :python! a b] :: #'cmd
  ;; minor mode map binding:
  [ :yas& a b] :: #'cmd
  ;; state map binding
  [ :normal? a b ] :: #'cmd
  ;; Combined:
  [ :python! :insert? a b] :: #'cmd
  ;; Separated
  [ :python! :insert? :|: a b ] :: #'cmd

  ;; local map override
  [ :python! :local a b ] :: #'cmd

  ;; Use a bloodbind group: ?
  [ (:bb decl-mode ) ] => :current
  )

(bloodbind! decl-mode ()
  "docstring for map closure"
  ;; set the map(s) for everything that comes after
  :let $_ = [ :python! :insert ] ;; implicit head pattern
  ;; This is bound to [python-mode-map a b]
  [ _ a b ] :: #'cmd
  :let $_ = nil ;; remove/change  head pattern
  :let $_ = []
  )

(bloodbind! aprofile (:profile)
  "docstring of a profile"
  [a b] :: #'cmd
  )

(bloodbind! wildcards ()
  ""
  ;; mode wildcards
  [ :*! a b] :: #'cmd
  ;; shallow key wildcards
  [ a b ,* e] :: #'cmd ;; match [a b c e], [a b d e], but not [a b q w e]
  ;; deep key wildcards
  [ a b ,** e] :: #'cmd ;; match [a b c e] and [a b q w e]

  ;; ranges
  [ a b <a-z> ] :: #'cmd ;; apply to those in the range
  ;; state wildcards
  [ :python! (:state *) ] :: #'escape

  )

(bloodbind! bindings ()
  ""
  ;; global reusable pattern binding to :keywords
  [a b c] -> :ex
  [:ex d] :: #'cmd ;; [a b c d] :: #'cmd

  ;; local bindings, only this bloodbind! block
  :let [x y z] -> :var
  [ :var q ] :: #'cmd

  ;; thus, leader keys:
  [SPC] -> :leader
  [:leader x y] :: #'cmd

  ;; :{}!-mode-map expansion
  [ :emacs-lisp! a b ] :: #'cmd
  ;; :{}&-minor-mode-map expansion
  [ :yas& a b ] :: #'cmd
  ;; literal map, compilation will create a map, then apply it to the variable later
  [ (:map quickrun--mode-map)  q ] :: #'cmd
  )

(bloodbind! late-binding ()
  ""
  ;; collection of matching bindings into hat variables as a global submap
  [ a b ,* ] => :^shallowgroup
  ;; all subbinds into submap
  [ a b ,**] => :^deepgroup
  ;; or local submaps
  let [a b ,*] => :^localgroup
  ;; submaps with metadata
  let [a b ,*] => :^submap (:desc "blah")

  ;; extension of submaps
  let [:^group ** ] ==> :^union ;; all of group-map goes into union-map
  ;; soft update, don't try to conflict
  let [:^other w **] +=> :^union ;; all of other-map's w bindings go into union-map
  ;; hard update, override the conflict if there is one
  let [r] :: #'cmd !=> :^union ;; plus an r binding goes into union-map

  [:^union ** ] ==> :^big;; becomes global big-map
  )

(bloodbind! state ()
  ""
  [ :python! (:state normal) x ] :: #'cmd
  ;;or
  [ :python! :normal? x ] :: #'cmd
  ;; with vars
  [ (:state normal) ] -> :n
  [ :python! :n x ] :: #'cmd

  ;; LATER declare new state if necessary
  [ :python! (:state blah ) x ] :: #'cmd
  [ :python!:n y ] :: #'evil-blah-state

  )

(bloodbind! conflict ()
  ""
  [a] :: #'cmd
  [a] :: #'cmd2 ;; <- error
  [!! a] :: #'cmd2 ;; override

  ;; Disable a pattern
  [a] :: nil
  ;; LATER Disallow a pattern, error at compile time
  [~ a] :: error
  [a] :: #'cmd ;; will error at compile
  [(~ :key x) a] :: ignore ;; require the binding have the key
  [a] :: #'cmd ;; will ignore the binding
  [(:key x) a] :: #'cmd ;; will store the binding
  )

(bloodbind! meta ()
  ""
  ;; just a plist after the cmd
  [ a b c ] :: #'cmd (:desc "blah" :ref val)
  ;; LATER conditionals, checked at keymap compile time
  [ a b d ] :: #'cmd (:when  #'(lambda () ) )
  [ a b d ] :: #'cmd (:unless #'(lambda () ) )
  )

(bloodbind! generation ()
  ""
  ;; apply a submap var
  [:emacs-lisp: q] :: :^group

  )

(bloodtransform! [pattern] ()
  ;; (λ list -> list)
  ;; eg: convert bindings into a transient,
  ;; just return the transient entry point
  )


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    July 24, 2024
;; Modified:   July 24, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; dsl_examples.el ends here
