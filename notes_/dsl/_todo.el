;;; _todo.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; setup standard patterns, eg:
;; - number keys
;; - alphabet
;; - motion
;; - read-expression/minibuffer?


;; Subtypes: text / special...
(bloodbind! changes ()

  ;; partial root
  :let [ :root^ ... ] -> 'prefix
  [a b m] :: #'change-buffer

  ;; partial root.text
  :let [ :text^ ... ] -> 'prefix
  [ ++ :root^ ++ ] => ;; text uses root
  [a b c] :: #'change-text

  ;; parital root.special
  :let [ :special^ ... ] -> 'prefix
  [ ++ :root^ ++ ] => ;; special also uses root
  [a b c] :: #'change-file
  )

;;; _todo.el ends here
