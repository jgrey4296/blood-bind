;;; blood_bind_macros.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(eval-when-compile
  (require 'cl-lib)
  (require 'jg-misc-macros)
  (require 'blood-bind--structs)
  (require 'blood-bind--compile)
  )

(cl-defmacro bloodbind! (name (&optional type)
                              &optional docstr
                              &rest body
                              &key override
                              &allow-other-keys
                              )
  "Adds entries into the a profile in the registry
using the bloodbind DSL.
Doesn't check for conflicts, or do expansions.

DSL:
{pattern} {op} {target} {meta}?
:let ({$var} = {val} ... )

Pattern:
[ {meta} :|: {key seq} ]

Pattern Key Seqs:
[ a b c ]                ;; vector of keys
[ ... $var    ... ]      ;; let variable expansion
[ ... :state? ... ]      ;; evil state
[ _  ... ]               ;; implicit variable state

Pattern Meta:
[ _ ]                    ;; implicit variable state
[ #'x ]                  ;; Remap cmd
[ :majname! ... ]        ;; mode map
[ :minname& ... ]        ;; mode map
[ (:kw val) ... ]        ;; Explicit/extendable meta
[ !!        ... ]        ;; Override
[ :local    ... ]        ;; local map pattern

Operator:
::                ;; keys to cmd
->                ;; var bind
=>                ;; pattern to submap | parent to submap

Target:
#'cmd             ;; commands
#'(lambda () )    ;; lambdas
:^map             ;; submaps

Meta:
( :kwd val ... )             ;; plists (extendable)
( :toggle val | mode )       ;;
( :hook hook fn )            ;;
( :on-compile val )          ;;
( :desc str )                ;;
( :allow-override bool )     ;;

Variables:
$var         ;;
$_           ;; implicit pattern leader?

Values:
:^submap     ;;
[ pattern ]  ;;

 "
  (declare (indent defun))
  (let* ((source (macroexp-file-name))
         (docstring (pcase docstr ;; if docstring, use it
                      ((pred stringp) docstr)
                      (x ;; else its part of the body
                       (push x body)
                       nil
                       )))
         (namesym (gensym! 'bloodbind name)) ;; gen name to register under
         (bodysym (gensym "body"))
         (clean-body (pop-plist-from-body! body))
         check   ;; unless-check
         )
    ;; assert type = bind | profile
    `(make-blood-bind-profile ,namesym ,docstring ,source ,clean-body)
      )
    )

(cl-defmacro bloodform! (name (&optional type)
                                   &optional docstr
                                   &rest body
                                   &key override
                                   &allow-other-keys)
  "Declare compilation time transforms of patterns.
Types :
(pattern) = [pattern]  -> [pattern]
(entry)   = entry-spec -> lambda
(map)     = maps*      -> ([pattern] -> [pattern])*
(token)   = [ token ]  -> token | kwd | map
"
  (declare (indent defun))
  (let* ((source (macroexp-file-name))
         (clean-body (pop-plist-from-body! body))
         )

    `(unless ,@unlesscheck

       )
    )
  )

;; util ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bbm-build-profile (name &rest body) ;; -> bbs-profile

  )

(defun bbm-build-transform () ;; -> bbs-transform

  )

(provide 'blood-bind--macros)

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
;;; blood_bind_macros.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("bbm-" . "blood-bind--macros-")
;; ("bbs-" . "blood-bind--structs-")
;; ("make-bb-" . "make-blood-bind-")
;; )
;; End:
