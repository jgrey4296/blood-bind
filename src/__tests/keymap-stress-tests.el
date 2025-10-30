;;; keymap-stress-tests.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header
(require 'buttercup)
(require 'blood-bind--buttercup)

(defvar keymap-stress-size 1000)

(defun gen-keyseqs (size form)
  (cl-loop for x from 0 upto size
           collect (funcall form x)))

(defun multi-keystr (x)
  (string-join (mapcar (-partial #'make-string 1)
                       (list
                        (+ ?a (% x 26))
                        (+ ?a (% x 26) 1)
                        (+ ?a (% x 26) 2)
                        )
                       )
               " ")
  )

(defun multi-keyvec (x)
  (vector (+ ?a (% x 26))
          (+ ?a (% x 26) 1)
          (+ ?a (% x 26) 2)
          )
  )

(defun solo-keystr (x)
  (make-string 1 (+ ?a (% x 26)))
)

(defun solo-keyvec (x)
  (make-string 1 (+ ?a (% x 26)))
)


(describe "single sequence string keymap-set stress"
  ;; Vars:
  :var (strings)
  (before-all
    (setq strings (gen-keyseqs keymap-stress-size #'solo-keystr))
    )
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "sets all the strings len 10"
    (let ((map (make-sparse-keymap))
          (len 10)
          )
      (dolist (x (seq-subseq strings 0 len))
        (keymap-set map x #'test)
        )
      )
    )
  (it "sets all the string len 100"
    (let ((map (make-sparse-keymap))
          (len 100)
          )
      (dolist (x (seq-subseq strings 0 len))
        (keymap-set map x #'test)
        )
      )
    )
  (it "sets all the string len 1000"
    (let ((map (make-sparse-keymap))
          (len 1000)
          )
      (dolist (x (seq-subseq strings 0 len))
        (keymap-set map x #'test)
        )
      )
  )

)

(describe "single sequence vector define-key stress"
  ;; Vars:
  :var (strings)
  (before-all
    (setq strings (gen-keyseqs keymap-stress-size #'solo-keyvec))
    )
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "sets all the strings len 10"
    (let ((map (make-sparse-keymap))
          (len 10)
          )
      (dolist (x (seq-subseq strings 0 len))
        (define-key map x #'test)
        )
      )
    )
  (it "sets all the string len 100"
    (let ((map (make-sparse-keymap))
          (len 100)
          )
      (dolist (x (seq-subseq strings 0 len))
        (define-key map x #'test)
        )
      )
    )
  (it "sets all the string len 1000"
    (let ((map (make-sparse-keymap))
          (len 1000)
          )
      (dolist (x (seq-subseq strings 0 len))
        (define-key map x #'test)
        )
      )
    )

)

(describe "single sequence string define-key stress"
  ;; Vars:
  :var (strings)
  (before-all
    (setq strings (gen-keyseqs keymap-stress-size #'solo-keystr))
    )
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "sets all the strings len 10"
    (let ((map (make-sparse-keymap))
          (len 10)
          )
      (dolist (x (seq-subseq strings 0 len))
        (define-key map x #'test)
        )
      )
    )
  (it "sets all the string len 100"
    (let ((map (make-sparse-keymap))
          (len 100)
          )
      (dolist (x (seq-subseq strings 0 len))
        (define-key map x #'test)
        )
      )
    )
  (it "sets all the string len 1000"
    (let ((map (make-sparse-keymap))
          (len 1000)
          )
      (dolist (x (seq-subseq strings 0 len))
        (define-key map x #'test)
        )
      )
    )

)

(describe "multi sequence string keymap-set stress"
  ;; Vars:
  :var (strings)
  (before-all
    (setq strings (gen-keyseqs keymap-stress-size #'multi-keystr))
    )
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "sets all the strings len 10"
    (let ((map (make-sparse-keymap))
          (len 10)
          )
      (dolist (x (seq-subseq strings 0 len))
        (keymap-set map x #'test)
        )
      )
    )
  (it "sets all the string len 100"
    (let ((map (make-sparse-keymap))
          (len 100)
          )
      (dolist (x (seq-subseq strings 0 len))
        (keymap-set map x #'test)
        )
      )
    )
  (it "sets all the string len 1000"
    (let ((map (make-sparse-keymap))
          (len 1000)
          )
      (dolist (x (seq-subseq strings 0 len))
        (keymap-set map x #'test)
        )
      )
    )

)

(describe "multi sequence vector define-key stress"
  ;; Vars:
  :var (strings)
  (before-all
    (setq strings (gen-keyseqs keymap-stress-size #'multi-keyvec))
    )
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "sets all the strings len 10"
    (let ((map (make-sparse-keymap))
          (len 10)
          )
      (dolist (x (seq-subseq strings 0 len))
        (define-key map x #'test)
        )
      )
    )
  (it "sets all the string len 100"
    (let ((map (make-sparse-keymap))
          (len 100)
          )
      (dolist (x (seq-subseq strings 0 len))
        (define-key map x #'test)
        )
      )
    )
  (it "sets all the string len 1000"
    (let ((map (make-sparse-keymap))
          (len 1000)
          )
      (dolist (x (seq-subseq strings 0 len))
        (define-key map x #'test)
        )
      )
    )

)


;;; keymap-stress-tests.el ends here
