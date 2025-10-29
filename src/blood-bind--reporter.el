;;; blood_bind_reporter.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defun bbr-conflict (&rest conflicts)
  " report on conflicting patterns for debugging "
  (error "Not Implemented")
 )

(defun bbr-overview-profiles ()
  "Report on all registered profiles"

  )

(defun bbr-overview-entries ()
  "report on all entries"

  )

(defun bbr-overview-transforms ()
  "report on all registered transforms"

  )

(defun bbr-overview-maps ()
  "report on all maps bb has control over"
  )

(defun bbr-dsl ()
  "Generate a description of the DSL"
  )

(defun bbr-check-map (the-map)
  " Take a keymap and print out all meta keys of the map "
  (cl-assert (keymapp the-map))
  (let ((c-xs (cl-loop for key in (number-sequence ?a ?z)
                       when (lookup-key the-map (kbd (format "C-%s" (char-to-string key))))
                       collect
                       (format "C-%s" (char-to-string key))))
        (m-xs (cl-loop for key in (number-sequence ?a ?z)
                       when (lookup-key the-map (kbd (format "M-%s" (char-to-string key))))
                       collect
                       (format "M-%s" (char-to-string key))))
        (cm-xs (cl-loop for key in (number-sequence ?a ?z)
                       when (lookup-key the-map (kbd (format "C-M-%s" (char-to-string key))))
                       collect
                       (format "C-M-%s" (char-to-string key))))
        ;; todo: also handle S, and mouse-1, mouse-2
        )

    (message "C-?'s: ")
    (mapc (lambda (x) (message "%s " x)) c-xs)
    (message "--------------------")
    (message "M-?'s: ")
    (mapc (lambda (x) (message "%s" x)) c-xs)
    (message "--------------------")
    (message "C-M-?'s: ")
    (mapc (lambda (x) (message "%s" x)) c-xs)
    (list c-xs m-xs cm-xs)
    )
  )

(defun bbr-list-all-maps ()
  " Get a list of all keymaps "
  (interactive)
  (let (allmaps)
    (cl-do-symbols (sym)
      (when (or (keymapp sym)
                (and (s-matches? "-map$" (symbol-name sym)) (not (functionp sym))))
        (push sym allmaps)
        )
      )
    (message "There are %s keymaps" (length allmaps))
    allmaps
    )
  )

(defun bbr-name-all-maps ()
  " Loop over all symbols and get any that are keymaps,
    Message the names of all those maps
 "
  (interactive)
  (let ((maps (current-active-maps))
        (mapcount 0)
        all-maps
        )
    (cl-do-all-symbols (sym)
      (when (and (keymapp (ffap-symbol-value sym))
                 (sequencep (ffap-symbol-value sym))
                 )
        (cl-incf mapcount)
        (push (symbol-name sym) all-maps)
        (set sym (append (symbol-value sym) `((:name ,sym))))
        )
      )
    all-maps
    )
  )

(provide 'blood-bind--reporter)
;;; blood_bind_reporter.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("bbr-" . "blood-bind--reporter-")
;; )
;; End:
