;;; entry-meta.el -*- lexical-binding: t; no-byte-compile: t; -*-

(bloodbind! metadata ()
  "Entries can have metadata"

  [a b c] :: #'cmd
  [a b d] :: (:toggle 'blah)
  [a b e] :: (:hook 'blah 'bloo-hook)
  [a b f] :: (#'cmd-other :desc "blah bloo")

  [a b f] :: (:map 'blah :desc "aweg")
  )


;;; entry-meta.el ends here
