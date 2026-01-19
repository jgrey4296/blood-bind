;;; transforms.el -*- lexical-binding: t; no-byte-compile: t; -*-

(bloodform! kw-macros ()
  "kw expansion macros definitions"

  [ :{}! ]     -> (:map "{}-mode-map")
  [ :{}& ]     -> (:map "{}-minor-mode-map")
  [ :{}^ ]     -> (:map "{}-map")
  [ :{}? ]     -> (:state "evil-{}-state-map")

  [ !! _ ]     -> (:override t)
  [ _ !! ]     -> (:override nil)
  [ _ ... ]    -> (:partial t)
  [ :remap ]   -> (:remap t)

  [ a b ]      -> [ a a b b]
  )




;;; transforms.el ends here
