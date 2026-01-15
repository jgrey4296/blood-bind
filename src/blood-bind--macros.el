;;; blood_bind_macros.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'macro-tools)
  (require 'blood-bind--vars)
  (require 'blood-bind-structs)
  )

(cl-defmacro bloodbind! (name (&rest args)
                              &optional docstr
                              &rest body
                              &key override type
                              &allow-other-keys
                              )
  "Adds entries into the a profile in the registry
using the bloodbind DSL.
Doesn't check for conflicts, or do expansions.

DSL:
{keys}
:let ({$var} = {val} ... )
{pattern} {op} {target} {meta}?

Keys:

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
         (docstring (pcase docstr
                      ;; if docstring, use it
                      ((pred stringp) docstr)
                      ;; else its part of the body
                      (x (push x body) nil)))
         (namesym (gensym! 'bloodbind name)) ;; gen name to register under
         (entrysym (gensym "entries"))
         (collsym (gensym "collection"))
         (clean-body (pop-plist-from-body! body))
         (unlesscheck `(unless (or ,override (featurep (quote ,blood-bind--delay-symbol)))))
         )
    (if (equal clean-body '(nil))
        nil
      `(,@unlesscheck
        (let* ((,entrysym (make-blood-bind-entries ,source ,clean-body))
               (,collsym  (make-blood-bind-collection ,namesym
                                                      ,docstring
                                                      ,entrysym
                                                      ,clean-body
                                                      ,args
                                                      ,type))
               )
          (blood-bind--register-collection ,collsym)
          )
        )
      )
    )
  )

(cl-defmacro bloodform! (name ()
                              &optional docstr
                              &rest body
                              &key override
                              &allow-other-keys)
  "Declare compilation time transforms of patterns.
(pattern) = [pattern]  -> [pattern] | fn | symbol | list
"
  (declare (indent defun))
  (let* ((source (macroexp-file-name))
         (docstring (pcase docstr
                      ;; if docstring, use it
                      ((pred stringp) docstr)
                      ;; else its part of the body
                      (x (push x body) nil)))
         (clean-body (pop-plist-from-body! body))
         (entrysym (gensym "entries"))
         (collsym (gensym "collection"))
         (unlesscheck `(unless (or ,override (featurep (quote ,blood-bind--delay-symbol)))))
         )
    `(,@unlesscheck
      (let* ((,entrysym (make-blood-bind-transforms ,source ,clean-body))
             (,collsym  (make-blood-bind-collection ,name
                                                    ,docstr
                                                    ,source
                                                    ,entrysym
                                                    nil
                                                    'transforms
                                                    ))
             )
        (blood-bind--register-collection ,collsym)
        )
      )
    )
  )

(provide 'blood-bind--macros)
;;; blood_bind_macros.el ends here
