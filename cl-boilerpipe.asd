;;;; cl-boilerpipe.asd

(asdf:defsystem #:cl-boilerpipe
  :serial t
  :description "Content extractor"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:serapeum
               #:anaphora
               #:optima
               #:cxml-stp
               #:xpath
               #:puri
               #:drakma
               #:css-selectors
               #:html5-sax
               #:sax-sanitize)
  :components ((:file "package")
               (:file "util")
               (:file "sanitize")
               (:file "cl-boilerpipe")))
