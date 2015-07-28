;;;; package.lisp

(defpackage #:cl-boilerpipe
  (:use #:cl #:alexandria #:serapeum #:anaphora #:optima)
  (:export #:strip-boilerplate #:strip-boilerplate/raw)
  (:import-from #:css-selectors #:query1)
  (:nicknames #:boilerpipe))
