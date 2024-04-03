;;; blood_bind_dsl.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;; basic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bloodbind!
 "a docstring"
 ;; General form: [pattern] :: #'cmd (:metadata)*
 ;; global, stateless bindings:
 [a] :: #'cmd
 ;; auto wrap lambdas with (interactive)
 [b] :: #'(lambda (x) (message "blah"))

 ;; auto mode toggles
 [ t q ] :: (:toggle global-flycheck-mode)
 ;; call function on compile for bind cmd/target
 [ t e] :: (:on-compile (transient-make-call! evil-embrace "E"))
 )

;; pattern wildcards ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bloodbind!
 ;; mode wildcards
 [ :*! a b] :: #'cmd
 ;; shallow key wildcards
 [ a b ,* e] :: #'cmd ;; match [a b c e], [a b d e], but not [a b q w e]
 ;; deep key wildcards
 [ a b ,** e] :: #'cmd ;; match [a b c e] and [a b q w e]

 ;; ranges
 [ a b ,<a-z> ] :: #'cmd ;; apply to those in the range
 ;; state wildcards
 [ :python: (:state ,*) ] :: #'escape

 )

;; binding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bloodbind!
 ;; global reusable pattern binding to :keywords
 [a b c] -> :ex
 [:ex d] :: #'cmd ;; [a b c d] :: #'cmd

 ;; local bindings, only this bloodbind! block
 let [x y z] -> :var
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

;; state patterns ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bloodbind!
 [ :python! (:state normal) x ] :: #'cmd
 ;;or
 [ :python! :normal% x ] :: #'cmd
 ;; with vars
 [ (:state normal) ] -> :n
 [ :python! :n x ] :: #'cmd

 ;; declares new state if necessary
 [ :python! (:state blah ) x ] :: #'cmd
 [ :python!:n y ] :: #'evil-blah-state

 )

;; conflicts and overrides ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bloodbind!
 [a] :: #'cmd
 [a] :: #'cmd2 ;; <- error
 [!! a] :: #'cmd2 ;; success?

 ;; Disable a pattern entirely

)

;; metadata ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bloodbind!
 ;; just a plist after the cmd
 [ a b c ] :: #'cmd (:desc "blah" :ref val)
 ;; conditionals, checked at keymap compile time
 [ a b d ] :: #'cmd (:if #'(lambda () ) )
)

;; generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bloodbind!
 ;; apply a submap var
 [:emacs-lisp: q] :: :^group

 )

;; transform ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bloodtransform! [pattern] ;; (λ list -> list)
                 ;; eg: convert bindings into a transient,
                 ;; just return the transient entry point
  )

;; debugging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'blood_bind_dsl)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    April 03, 2024
;; Modified:   April 03, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; blood_bind_dsl.el ends here
