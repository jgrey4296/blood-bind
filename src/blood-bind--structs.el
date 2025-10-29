;;; blood_bind_structs.el -*- lexical-binding: t; no-byte-compile: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'seq)
  (require 'blood-bind--vars)
  )

;; structs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (blood-bind--store
               (:constructor nil)
               (:constructor make--blood-bind-store-internal)
               )
  " The main store of all profiles, pre- and post- compilation "
  (profiles     (make-hash-table)    :type 'hash-table)
  (collections  (make-hash-table)    :type 'hash-table)
  (transforms   (make-hash-table)    :type 'hash-table)
  (compiled     (make-hash-table)    :type 'hash-table)
  (partial-maps (make-hash-table)    :type 'hash-table)
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
  (locals  nil    :type 'list   :read-only t)
  (globals nil    :type 'list   :read-only t)
  )

(cl-defstruct (blood-bind--compiled
               (:constructor nil)
               (:constructor make--blood-bind-compiled-internal)
               )
  " A compiled profile of bindings.
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
  (pattern  nil  :type 'blood-bind--pattern  :read-only t)
  (operator nil  :type 'symbol               :read-only t)
  (target   nil  :type (or 'keyword 'symbol) :read-only t)
  (meta     nil  :type 'plist                :read-only t)
  (source   nil  :type 'str                  :read-only t)
  (expanded nil  :type 'bool                 :read-only t)
  )

(cl-defstruct (blood-bind--transform
               (:constructor nil)
               (:constructor make--blood-bind-transform-internal))
  "Describes a registered transform "
  (pattern     nil :type 'blood-bind--pattern        :read-only t)
  (replacement nil :type (or 'symbol 'pattern) :read-only t)
  (sort        nil :type 'int                        :read-only t)
  (source      nil :type 'str                        :read-only t)
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

;; Ctors and util;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-blood-bind-collection (name docstr source entries &optional globals type) ;; -> collection
  "Make and register a collection of binding entries"
  (cl-assert (symbolp name))
  (cl-assert (stringp docstr))
  (cl-assert (stringp source))
  (cl-assert (plistp entries))
  (cl-assert (cl-every #'symbolp globals))
  (cl-assert (and (or (null type)
                      (symbolp type))
                  (-contains-p
                   (mapcar #'car (cdr (cl-struct-slot-info 'blood-bind--store)))
                   (or type 'collections))))
  (unless blood-bind--registry (setq blood-bind--registry (make--blood-bind-store-internal)))
  (puthash name (make--blood-bind-collection-internal :name name
                                                      :docstr docstr
                                                      :source source
                                                      :type (or type 'collections)
                                                      :entries (plist-get entries :entries)
                                                      :locals (plist-get entries :locals)
                                                      :globals globals
                                                      )
           (cl-struct-slot-value 'blood-bind--store
                                 (or type  'collections)
                                 blood-bind--registry)
           )
  )

(defun make-blood-bind-entries (source raw) ;; -> plist(:locals :entries)
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
        ((and x (pred keywordp) (guard (-contains-p bbv-entry-type-kwds x)))
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
  (let ((pattern  (bbs-parse-pattern lhs))
        (operator (bbs-parse-op op))
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

(defun make-blood-bind-transforms (source raw) ;; -> plist(:entries list[transform])
  "build a transfrom description"
  (let (transforms
        )
    (while raw
      (pcase (seq-first raw)
        ((and x (pred blood-bind--transform-p))
         (push x transforms)
         (setq raw (seq-rest raw)))
        ((pred vectorp)
         (cl-destructuring-bind (patt op target) (seq-take raw 3)
           (push (make--blood-bind-transform-internal
                  :pattern (bbs-parse-pattern patt)
                  :replacement (pcase target
                                 ((pred vectorp)
                                  (bbs-parse-pattern target))
                                 ((pred symbolp)
                                  target)
                                 )
                  :source source
                  )
                 transforms)
           )
         (setq raw (seq-drop raw 3))
         )
        (x (signal 'blood-bind-parse-error 'transforms x))
        )
      )
    (list :entries (reverse transforms))
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
   ((eq bbv-lhs-sep-sym val) t)
   ;; convert shorthands
   ((alist-get val bbv-lhs-symbol-alist)
    `(,(alist-get val bbv-lhs-symbol-alist) t))
   ;; recognize remaps
   ((and (consp val) (eq (car-safe val) 'function)) '(:remap t))
   ;; recognize boolean kwds
   ((and (keywordp val) (-contains-p bbv-lhs-patt-bool-kwds val))
    `(,val t))
   ;; recognize valued cons'
   ((and (keywordp (car-safe val)) (-contains-p bbv-lhs-patt-val-kwds (car-safe val))) val)
   ;; convert complex keywords
   ((keywordp val)
    (pcase (alist-get (substring (symbol-name val) -1) bbv-lhs-kwd-suffix-alist nil nil 'equal)
      ('nil nil)
      (`(,x ,y) `(,x ,(intern (format "%s%s" (substring (symbol-name val) 1 -1) (or y "")))))
      ))
   ;; otherwise nil
   (t nil)
   )
  )

(defun bbs-parse-op (op) ;; -> maybe[kwd]
  (cl-assert (symbolp op) nil "op needs to be a symbol")
  (alist-get op bbv-op-symbol-alist)
  )

(defun bbs-pattern-key-parse (val) ;; -> list
  " "

  )

(provide 'blood-bind--structs)
;;; blood_bind_structs.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("bbs-" . "blood-bind--structs-")
;; ("bbv-" . "blood-bind--vars-")
;; )
;; End:
