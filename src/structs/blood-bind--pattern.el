;;; blood-bind--pattern.el -*- lexical-binding: t; no-byte-compile: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'seq)
  (require 'dash)
  (require 'blood-bind--vars)
  )

(defvar blood-bind--lhs-symbol-alist   (list '(_ . :implicit-prefix)
                                             '(\* . :glob)
                                             '(? . :insert)
                                             '(!! . :override)
                                             )
  "A reverse Plist to remap shorthand pattern symbols to their bool keywords"
  )

(defvar blood-bind--lhs-patt-bool-kwds  (list :root :remap :local :implicit-prefix))

(defvar blood-bind--lhs-patt-val-kwds   (list :map :state :remap :local))

(defvar blood-bind--lhs-kwd-suffix-alist (list
                                               '("!" . (:map "-mode-map"))
                                               '("&" . (:map "-minor-mode-map"))
                                               '("^" . (:map "-map"))
                                               '("?" . (:state nil))
                                               )
  "Alist for converting a suffix of a keyword"
  )

(defvar blood-bind--lhs-sep-sym :|:  "Symbol that separates metadata from keybinding")

;;--------------------------------------------------

(cl-defstruct (blood-bind--pattern
               (:constructor nil)
               (:constructor make-blood-bind--pattern))
  " a binding pattern that will be compiled into a keymap
eg: ('python-mode-map 'normal
"
  (keys     nil   :type 'vector :read-only t)
  (meta     nil   :type 'plist  :read-only t)
  )

;;--------------------------------------------------

(defun blood-bind--parse-pattern (pattern) ;; -> pattern
  (cl-assert (vectorp pattern) nil "Pattern must be a vector")
  (cl-assert (not (seq-empty-p pattern)) nil "pattern must have elements")
  (let ((keys (vconcat pattern))
        (meta (list)) ;; <- plist of recognized meta data
        )
    ;; Pop off leading meta data
    (while (and (not (seq-empty-p pattern))
                (pcase (blood-bind--parse-meta (seq-first pattern))
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

    (make-blood-bind--pattern
     :keys pattern
     :meta (-flatten meta)
     )
    )
  )

(defun blood-bind--parse-meta (val) ;; -> t | maybe[(kwd val)]
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
   ((eq blood-bind--lhs-sep-sym val) t)
   ;; convert shorthands
   ((alist-get val blood-bind--lhs-symbol-alist)
    `(,(alist-get val blood-bind--lhs-symbol-alist) t))
   ;; recognize remaps
   ((and (consp val) (eq (car-safe val) 'function)) '(:remap t))
   ;; recognize boolean kwds
   ((and (keywordp val) (-contains-p blood-bind--lhs-patt-bool-kwds val))
    `(,val t))
   ;; recognize valued cons'
   ((and (keywordp (car-safe val)) (-contains-p blood-bind--lhs-patt-val-kwds (car-safe val))) val)
   ;; convert complex keywords
   ((keywordp val)
    (pcase (alist-get (substring (symbol-name val) -1) blood-bind--lhs-kwd-suffix-alist nil nil 'equal)
      ('nil nil)
      (`(,x ,y) `(,x ,(intern (format "%s%s" (substring (symbol-name val) 1 -1) (or y "")))))
      ))
   ;; otherwise nil
   (t nil)
   )
  )

(defun blood-bind--parse-key-sequence (val) ;; -> list
  " "

  )

(provide 'blood-bind--pattern)
;;; blood-bind--pattern.el ends here
