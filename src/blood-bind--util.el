;;; blood_bind_utils.el -*- lexical-binding: t; -*-

;; '((a . b) (c . d))
;; car is an activation value/variable
;; cdr is a keymap
(defvar blood-bind-emulation-alist (list))

(defun blood-bind-make-quit-map () ;; -> keymap
  "Create a keymap, with one binding: quit"
  (make-sparse-keymap "blah")
  )

(defun blood-bind-make-insert-map () ;; -> keymap
  "create a keymap where every binding is #'self-insert-command"
  (make-keymap)
  )

(defun blood-bind-ensure-emulation-alist (&optional onlyp)
  "ensure blood-bind-emulation-alist is in 'emulation-mode-map-alists

if onlyp, remove any other emulation modes
"
  (when onlyp (setq emulation-mode-map-alists (list)))
  (push 'blood-bind-emulation-alist emulation-mode-map-alists)
  )

(defun blood-bind-push-map (map) ;; map -> nil
  "Push a keymap to the front of bb-emulation-alist"
  (interactive)
  (push (cons t map) blood-bind-emulation-alist)
  )

(defun blood-bind-pop-map () ;; -> map
  "Pop a keymap off bb-emulation-alist"
  (interactive)
  (when blood-bind-emulation-alist
    (pop blood-bind-emulation-alist)
    )
  )

(defun blood-bind-install-default-global-map () ;; -> nil
  "Set the global map to the bb default global map"
  (use-global-map (blood-bind-make-quit-map))
  )

(defun blood-bind-valid-keymap-binding-p (val)
  "Check val is a suitable binding type according to:
https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Lookup.html
"

  )

(provide 'blood-bind--util)
;;; blood-bind--util.el ends here
