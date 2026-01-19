;; -*- mode: eask; lexical-binding: t -*-


(package "blood-bind"
         "0.1.0"
         "A keybinding framework")

(website-url "https://github.com/jgrey4296/blood-bind")
(keywords "bindings")

(package-descriptor "blood-bind-pkg.el")
(files
 "./src/blood-bind.el"
 "./src/blood-bind--*.el"
 "./src/[a-z]*/*.el"
 )
(load-paths "./src/" "./src/[a-z]*/")

(source "gnu")
(source "melpa")

(depends-on "emacs" "30.2")
(depends-on "dash" "2.20.0")
(depends-on "macro-tools"
            :repo "jgrey4296/macro-tools"
            :fetcher 'github
            )
(depends-on "eieio" "1.4")
(depends-on "s" "1.31.1")

(development
 (depends-on "magit")
 (depends-on "evil")
 )
