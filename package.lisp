;;;; package.lisp

(defpackage #:cl-boilerpipe
  (:use #:cl #:alexandria #:serapeum #:anaphora #:optima)
  (:export #:strip-boilerplate #:strip-boilerplate/raw)
  (:nicknames #:boilerpipe))
