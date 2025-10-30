;;; blood-bind--raw.el -*- lexical-binding: t; no-byte-compile: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'seq)
  (require 'dash)
  (require 'blood-bind--vars)
  )

(cl-defstruct (blood-bind--collection
               (:constructor nil)
               (:constructor make-blood-bind--collection)
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

;;--------------------------------------------------

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

  (unless blood-bind--registry (setq blood-bind--registry (make-blood-bind--store)))
  (puthash name
           (make-blood-bind--collection :name name
                                        :docstr docstr
                                        :source source
                                        :type (or type 'collections)
                                        :entries (plist-get entries :entries)
                                        :locals (plist-get entries :locals)
                                        :globals globals
                                        )
           (cl-struct-slot-value 'blood-bind--store
                                 (or type 'collections)
                                 blood-bind--registry)
           )
  )

(provide 'blood-bind--collection)
;;; blood-bind--raw.el ends here
