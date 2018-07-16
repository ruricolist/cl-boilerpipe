;;;; cl-boilerpipe.asd

(asdf:defsystem #:cl-boilerpipe
  :serial t
  :description "Content extractor"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:serapeum

               #:fxml #:fxml/stp
               #:fxml/cxml #:fxml/html5
               #:cxml-stp
               #:fxml/css-selectors
               #:fxml/sanitize

               #:cl-html5-parser
               #:anaphora)
  :components ((:file "package")
               (:file "util")
               (:file "sanitize")
               (:file "cl-boilerpipe")))
