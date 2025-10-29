;;; blood_bind_compile.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defun bbc-profile (profile) ;; bbs-profile -> bbs-compiled
  "Compiles a named profile with the segments it describes"
  (cl-assert (symbolp profile))
  (cl-assert (not (null blood-bind--registry)))
  (cl-assert (gethash profile (blood-bind--store-profiles blood-bind--registry)))

  ;; get profile
  ;; get global bindings
  ;; extract map-vars
  ;; build empty compiled-profile of maps
  ;; for (locals, entries) in profile:
  ;; ;; for entry in entries:
  ;; ;; ;; expand-entry(entry, using=locals, onto=comp-profile.lookup[entry.map])

  ;; return compiled profile
  (error "Not Implemented")
  )

(defun bbc-entry (entry) ;; bbs-entry -> list
  "Compile an entry into relevant partial maps"
  (cl-assert (not (null blood-bind--registry)))
  ;; get maps[entry.pattern.map|state] -> profilemap
  ;; entry.pattern.keys -> keymap vector
  ;; bind pmap[keyvec] = maps[entry.target] | entry.target

  (error "Not Implemented")
  )

(defun bbc-gen-maps () ;; -> hash-table[sym, keymap]
  "Generate the empty keymaps blood-bind will put bindings into,
from registered maps in `blood-bind--registry'
"
  (cl-assert (not (null blood-bind--registry)))
  (error "Not Implemented")
  )

(defun bbc-make-partial-map (name &optional strict) ;; -> keymap
  "Add a new partial map to the global registry and return it.
If the partial map already exists, just return it,
unless `strict', in which case error
"
  (cl-assert (not (null blood-bind--registry)))
  (when (and strict (gethash name (blood-bind--store-partial-maps blood-bind--registry)))
    (signal 'blood-bind-error name "already exists"))
  (unless (gethash name (blood-bind--store-partial-maps blood-bind--registry))
    (puthash name
             (make-sparse-keymap)
             (blood-bind--store-partial-maps blood-bind--registry)))

  (gethash name (blood-bind--store-partial-maps blood-bind--registry))
  )

(defun bbc-add-entry-to-partial (partial entry)
  "Add an entry to a given partial map"
  (cl-assert (or (symbolp partial) (keymapp partial)))
  (cl-assert (blood-bind--entry-p entry))
  (cl-assert (not blood-bind--vars-advice-active))
  (let ((pmap (pcase partial
                   ((pred symbolp)
                    (gethash partial (bbc-make-partial-map partial)))
                   ((pred keymapp)
                    partial)
                   ))
        (keys (bbc-compile-pattern-to-string (blood-bind--entry-pattern entry)))
        (target (blood-bind--entry-target entry))
        )
    (pcase (keymap-lookup pmap keys nil)
      ((or 'nil (pred numberp)) nil)
      ((and x (pred symbolp))
       (signal 'blood-bind-conflict-error (list partial keys x)))
      )
    (keymap-set pmap
                keys
                target
                )
    partial
    )
  )

(defun bbc-compile-pattern-to-string (pattern) ;; -> string
  "Convert a pattern to a key-valid-p string,
also running any registered transforms
"
  (cl-assert (blood-bind--pattern-p pattern))
  (let* ((keys (blood-bind--pattern-keys pattern))
         keystr
         )
    ;; TODO apply token level transforms here
    (setq keystr (string-join (mapcar #'symbol-name keys) " "))
    (cl-assert (key-valid-p keystr))
    keystr
    )
  )

(defun bbc-combine-partial-maps (&rest maps) ;; -> keymap
  "Having converted entries to partial maps, combine those partial maps"
  (cl-assert (cl-every #'keymapp maps))
  (keymap-canonicalize (make-composed-keymap maps))
  )

(defun bbc-get-collection-map-names (coll) ;; -> list[symbol]
  "From a collection, get all map names,
including those defined in:
- let bindings,
- states
- major mode
- minor mode
- explicitly
 "
  (cl-assert (blood-bind--collection-p coll))
  (let (result)
    (cl-loop for entry in (blood-bind--collection-entries coll)
             for meta  = (blood-bind--pattern-meta (blood-bind--entry-pattern entry))
             when (plist-get meta :map)
             do (add-to-list result (plist-get meta :map))
             when (plist-get meta :state)
             do (add-to-list result (plist-get meta :state))
             )
    result
    )
  )


(provide 'blood-bind--compile)
;;; blood_bind_compile.el ends here
;; Local Variables:
;; read-symbol-shorthands: (
;; ("bbc-" . "blood-bind--compile-")
;; ("bbv-" . "blood-bind--vars-")
;; )
;; End:
