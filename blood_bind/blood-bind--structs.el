;;; blood_bind_structs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(eval-when-compile
  (require 'cl-lib)
  (require 'seq)
  )

(defvar bbs-type-kwds           (list :profile ))

(defvar bbs-entry-type-kwds     (list :let :endlet))

(defvar bbs-lhs-patt-bool-kwds  (list :root :remap :local :implicit-prefix))

(defvar bbs-lhs-patt-val-kwds   (list :map :state :remap :local))

(defvar bbs-op-kwds             (list :let :bind :submap))

(defvar bbs-rhs-kwds            (list :toggle :hook :on-compile :desc :allow-override))

(defvar bbs-lhs-symbol-alist   (list '(_ . :implicit-prefix)
                                     '(\* . :glob)
                                     '(? . :insert)
                                     '(!! . :override)
                                     )
  "A reverse Plist to remap shorthand pattern symbols to their bool keywords"
  )

(defvar bbs-op-symbol-alist    (list '(-> . :let) '(:: . :bind) '(=> . :submap))
  "A reverse Plist to remap shorthand operator symbols to their keywords"
  )

(defvar bbs-lhs-kwd-suffix-alist (list
                                  '("!" . (:map "-mode-map"))
                                  '("&" . (:map "-minor-mode-map"))
                                  '("^" . (:map "-map"))
                                  '("?" . (:state nil))
                                  )
  "Alist for converting a suffix of a keyword"
  )

(defvar bbs-lhs-sep-sym :|:  "Symbol that separates metadata from keybinding")

(defconst bbs-major-map-suffix "!")

(defconst bbs-minor-map-suffix "&")

(defconst bbs-explicit-map-suffix "^")

(defvar blood-bind-global-store nil)

;; structs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-error 'blood-bind-error "General Blood Bind Error")
(define-error 'blood-bind-parse-error "Parsing an entry failed" 'blood-bind-error)

(cl-defstruct (blood-bind--store
               (:constructor nil)
               (:constructor make--blood-bind-store-internal)
               )
  " The main store of all profiles, pre- and post- compilation "
  (profiles (make-hash-table)    :type 'hash-table)
  (collections (make-hash-table) :type 'hash-table)
  (compiled (make-hash-table)    :type 'hash-table)
  )

(cl-defstruct (blood-bind--collection
               (:constructor nil)
               (:constructor make--blood-bind-collection-internal)
               )
  " a named collection of entries.
at compile: all(entries) -> (list major-list minor-list state-list )
then foreach sym: 'sym-profile-name-map
(eg: 'python -> python-profile-default-map)
on profile (apply default): (setq python-mode-map python-profile-default-map)

"
  (name    nil    :type 'symbol :read-only t)
  (docstr  nil    :type 'str    :read-only t)
  (source  nil    :type 'string :read-only t)
  (type    nil    :type 'symbol :read-only t)
  (entries nil    :type 'list   :read-only t)
  (globals nil    :type 'list   :read-only t)
  )

(cl-defstruct (blood-bind--compiled
              (:constructor nil)
               (:constructor make--blood-bind-compiled-internal)
               )
  " The compiled profile, with local maps
when applied, loops (setq maps.key maps.value)
"
  (name nil               :type 'symbol   :read-only t)
  (source nil             :type 'string   :read-only t)
  (maps (make-hash-table) :type 'hash-table)
  )

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
  (pattern  nil  :type 'blood-bind--pattern :read-only t)
  (operator nil  :type 'symbol              :read-only t)
  (target   nil  :type 'symbol-or-kw        :read-only t)
  (meta     nil  :type 'plist               :read-only t)
  (file     nil  :type 'str                 :read-only t)
  (expanded nil  :type 'bool                :read-only t)
  )

(cl-defstruct (blood-bind--pattern
               (:constructor nil)
               (:constructor make--blood-bind-pattern-internal))
  " a binding pattern that will be compiled into a keymap
eg: ('python-mode-map 'normal
"
  (keys     nil   :type 'vector :read-only t)
  (meta     nil   :type 'plist  :read-only t)
  )

(cl-defstruct (blood-bind--transform
               (:constructor nil)
               (:constructor make--blood-bind-transform-internal))
  "Describes a registered transform"
  (name nil :type 'symbol :read-only t)
  (type nil :type 'symbol :desc "pattern | entry | map | token" :read-only t)
  )

;; Ctors and util;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-blood-bind-profile (name source entries) ;; -> collection[profile]
  " make and register a collection that amounts to a profile "

  )

(defun make-blood-bind-collection (name docstr source entries) ;; -> collection[profile]
  "Make and register a collection of binding entries"
  (let (built-entries
        curr
        )
    (while entries
      (pcase (seq-first entries)
        ((and x (pred keywordp) (guard (-contains-p bbs-lhs-kwds x)))
         ;; let binding
         (setq curr (seq-take entries 4))
         (push (apply #'make-blood-bind-entry curr) built-entries)
         (setq entries (seq-drop entries 4)))
        ((pred vectorp)
         ;; normal binding
         (setq curr (seq-take entries 3))
         (push (apply #'make-blood-bind-entry curr) built-entries)
         (setq entries (seq-drop entries 3)))
        (x (signal 'blood-bind-parse-error x))
        )
      )
    (unless blood-bind-global-store (setq blood-bind-global-store (make--blood-bind-store-internal)))
    (puthash name (make--blood-bind-collection-internal :name name
                                                        :docstr docstr
                                                        :source source
                                                        :entries built-entries
                                                        )
             (blood-bind--store-collections blood-bind-global-store)
             )
    )
  )

(defun make-blood-bind-entry (source lhs op rhs &optional special) ;; -> entry
  "Main entry point to build entries, parsing the lhs"
  (let ((pattern  (bbs-parse-pattern lhs))
        (operator (bbs-parse-op op))
        target
        meta
        )
    (pcase rhs
      ((pred plistp)
       (setq target (plist-get rhs :target)
             meta   rhs
             )
       (cl-remf meta :target)
       )
      ((pred symbolp)
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
                                     :file     source
                                     :meta     meta
                                     )
    )
  )

(defun bbs-parse-pattern (pattern) ;; -> pattern
  (cl-assert (vectorp pattern) nil "Pattern must be a vector")
  (cl-assert (not (seq-empty-p pattern)) nil "pattern must have elements")
  (let ((keys (vconcat pattern))
        (meta (list)) ;; <- plist of recognized meta data
        )
    ;; Pop off leading meta data
    (while (and (not (seq-empty-p pattern))
                (pcase (bbs-pattern-meta-parse (seq-first pattern))
                  ('nil nil)
                  ('t
                   ;; parsed the separator
                   (setq pattern (seq-rest pattern))
                   nil)
                  ((and x (pred plistp))
                   ;; parsed actual data
                   (push x meta)
                   (setq pattern (seq-rest pattern))
                   t)
                  (x
                   ;; Fallback
                   nil)
                  ))
      )

    (make--blood-bind-pattern-internal
     :keys pattern
     :meta (-flatten meta)
     )
    )
  )

(defun bbs-pattern-meta-parse (val) ;; -> t | maybe[(kwd val)]
  "Discriminate the viable metadata elements of a pattern to a maybe keyword pair
#'fn       -> :remap t
:{kwd}!    -> (:map kwd-major-mode-map)
:{kwd}&    -> (:map kwd-minor-mode-map)
:{kwd}^    -> (:map kwd)
:{kwd}?    -> (:state kwd)
(:kwd val) -> (:kwd val)
"
  (cond
   ;; meta :|: keys
   ((eq bbs-lhs-sep-sym val) t)
   ;; convert shorthands
   ((alist-get val bbs-lhs-symbol-alist)
    `(,(alist-get val bbs-lhs-symbol-alist) t))
   ;; recognize remaps
   ((and (consp val) (eq (car-safe val) 'function)) '(:remap t))
   ;; recognize boolean kwds
   ((and (keywordp val) (-contains-p bbs-lhs-patt-bool-kwds val))
    `(,val t))
   ;; recognize valued cons'
   ((and (keywordp (car-safe val)) (-contains-p bbs-lhs-patt-val-kwds (car-safe val))) val)
   ;; convert complex keywords
   ((keywordp val)
    (pcase (alist-get (substring (symbol-name val) -1) bbs-lhs-kwd-suffix-alist nil nil 'equal)
      ('nil nil)
      (`(,x ,y) `(,x ,(intern (format "%s%s" (substring (symbol-name val) 1 -1) (or y "")))))
      ))
   ;; otherwise nil
   (t nil)
   )
  )

(defun bbs-parse-op (op) ;; -> maybe[kwd]
  (cl-assert (symbolp op) nil "op needs to be a symbol")
  (alist-get op bbs-op-symbol-alist)
  )

(defun bbs-pattern-key-parse (val) ;; -> list
  " "

  )

(provide 'blood-bind--structs)

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
;;; blood_bind_structs.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("bbs-" . "blood-bind--structs-")
;; )
;; End:
