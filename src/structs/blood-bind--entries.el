;;; blood-bind--entries.el -*- lexical-binding: t; no-byte-compile: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'seq)
  (require 'dash)
  (require 'blood-bind--vars)
  )

(defvar blood-bind--entry-type-kwds     (list :let :endlet))

(defvar blood-bind--op-kwds             (list :let :bind :submap))

(defvar blood-bind--rhs-kwds            (list :toggle :hook :on-compile :desc :allow-override))

(defvar blood-bind--op-symbol-alist    (list
                                        '(-> . :let)
                                        '(:: . :bind)
                                        '(=> . :submap)
                                        )
  "A reverse Plist to remap shorthand operator symbols to their keywords"
  )

;;--------------------------------------------------


(cl-defstruct (blood-bind--entry
               (:constructor nil)
               (:constructor make--blood-bind-entry-internal)
               )
  " a single binding entry. a pattern, its target, and metadata.
can be:
([pattern] #'cmd (meta)) : standard binding
([pattern]  :kw (:bind 'global)) : global binding
([pattern]  :kw (:bind 'local))  : local binding

 "
  (pattern  nil  :type 'blood-bind--pattern  :read-only t)
  (operator nil  :type 'symbol               :read-only t)
  (target   nil  :type (or 'keyword 'symbol) :read-only t)
  (meta     nil  :type 'plist                :read-only t)
  (source   nil  :type 'str                  :read-only t)
  (expanded nil  :type 'bool                 :read-only t)
  )

;;--------------------------------------------------

(defun make-blood-bind-entries (source raw) ;; -> plist(:locals :entries)
  "Makes a plist of entries, separated into:

:locals  - local let bindings of these entries
:entries - the entries themselves
 "
  (cl-assert (stringp source))
  (cl-assert (listp raw))
  (let (entries
        locals
        )
    (while raw
      (pcase (seq-first raw)
        ((and x (pred blood-bind--entry-p))
         (push (seq-first raw) entries)
         (setq raw (seq-rest raw)))
        ((and x (pred keywordp) (guard (-contains-p blood-bind--entry-type-kwds x)))
         ;; let binding
         (push (apply #'make-blood-bind-entry
                      source
                      (append (seq-rest (seq-take raw 4))
                              (list :let)))
               locals)
         (setq raw (seq-drop raw 4)))
        ((pred vectorp)
         ;; normal binding
         (push (apply #'make-blood-bind-entry
                        source
                        (seq-take raw 3))
               entries)
         (setq raw (seq-drop raw 3)))
        (x
         (signal 'blood-bind-parse-error x))
        )
      )
    (list :locals (reverse locals) :entries (reverse entries))
    )
  )

(defun make-blood-bind-entry (source lhs op rhs &optional special) ;; -> entry
  "Main entry point to build entries, parsing the lhs"
  (let ((pattern  (blood-bind--parse-pattern lhs))
        (operator (blood-bind--entries-parse-op op))
        target
        meta
        )
    (pcase rhs
      ((pred plistp)
       ;; meta plist
       (setq target (plist-get rhs :target)
             meta   rhs
             )
       (cl-remf meta :target)
       )
      ((pred symbolp)
       ;; target symbol
       (setq target rhs
             meta nil)
        )
      )
    (cl-assert (not (null operator)))
    (cl-assert (not (null target)))
    (cl-assert (or (consp target) (symbolp target)))
    (cl-assert (or (null meta) (plistp meta)))
    (cl-assert (stringp source)  t "Source must be a string")
    (cl-assert (or (listp rhs) (symbolp rhs)) t "rhs must be a symbol")
    (cl-assert (or (not special) (eq operator special)) "special entries have to match on the operator and prefix")
    (make--blood-bind-entry-internal :pattern  pattern
                                     :operator operator
                                     :target   rhs
                                     :source   source
                                     :meta     meta
                                     )
    )
  )

(defun blood-bind--entries-parse-op (op) ;; -> maybe[kwd]
  (cl-assert (symbolp op) nil "op needs to be a symbol")
  (alist-get op blood-bind--op-symbol-alist)
  )

(provide 'blood-bind--entries)
;;; blood-bind--entries.el ends here
