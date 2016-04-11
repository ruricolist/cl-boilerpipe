;;;; cl-boilerpipe.asd

(asdf:defsystem #:cl-boilerpipe
  :serial t
  :description "Content extractor"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:serapeum
               #:cxml #:cxml-stp #:css-selectors
               #:fxml #:fxml/stp #:fxml/cxml
               #:cl-html5-parser
               #:html5-sax
               #:sax-sanitize
               #:anaphora)
  :components ((:file "package")
               (:file "util")
               (:file "sanitize")
               (:file "cl-boilerpipe")))
