;;;; cl-boilerpipe.asd

(asdf:defsystem #:cl-boilerpipe
  :serial t
  :description "Content extractor"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:serapeum

               #:cxml #:cxml-stp

               #:fxml #:fxml/stp
               #:fxml/cxml #:fxml/html5
               #:fxml/css-selectors
               #:fxml/sanitize

               #:cl-html5-parser
               #:anaphora)
  :components ((:file "package")
               (:file "util")
               (:file "sanitize")
               (:file "cl-boilerpipe")))
