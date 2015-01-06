(in-package #:cl-boilerpipe)

(defconst xhtml-ns "http://www.w3.org/1999/xhtml")

(defvar *site-configs* '())

(defclass site-config ()
  ((domain :initarg :domain :accessor domain)
   (rules :initarg :rules :accessor rules))
  (:default-initargs :rules '()))

(defmethod print-object ((self site-config) stream)
  (print-unreadable-object (self stream :type t)
    (format stream (domain self))))

(defclass site-parse ()
  ((author :initarg :author :accessor author)
   (date :initarg :date :accessor date)
   (body :initarg :body :accessor body)
   (title :initarg :title :accessor title)))

(defun get-site-configs ()
  (labels ((get-files ()
             (fad:list-directory
              (asdf:system-relative-pathname :cl-boilerpipe "site-config/")))
           (get-domains (file)
             (~> file
                 namestring
                 (split-sequence #\/ _)
                 lastcar
                 words
                 butlast
                 (join _ ".")))
           (get-pattern (file)
             (~> file
                 read-file-into-string
                 lines
                 (remove-if (disjoin #'blankp
                                     (curry #'string^= "#"))
                            _)))
           (parse-pattern (pattern)
             (mapcar
              (lambda (rule)
                (let ((pos (position #\: rule)))
                  (cons (subseq rule 0 pos)
                        (trim-whitespace (subseq rule (1+ pos))))))
              pattern)))
    (let* ((files (get-files))
           (domains (mapcar #'get-domains files))
           (patterns (mapcar #'get-pattern files)))
      (loop for domain in domains
            for pattern in patterns
            collect (acons "domain" domain (parse-pattern pattern))))))

(defun make-site-config (rules)
  (lret ((conf (make 'site-config)))
    (loop for (k . v) in rules
          do (xpath:with-namespaces ((nil xhtml-ns))
               (labels ((rule (rule &key important)
                          (if important
                              (push rule (rules conf))
                              (appendf (rules conf) (list rule))))
                        (extract (expr slot &key (fn #'identity))
                          (let ((expr (xpath:compile-xpath expr)))
                            (lambda (node result)
                              (ensure (slot-value result slot)
                                (funcall fn (xpath:evaluate-compiled expr node))))))
                        (extract-string (expr slot)
                          (extract expr slot :fn #'xpath:string-value)))
                 (string-case k
                   ("domain"
                    (setf (domain conf) v))
                   ("strip"
                    (let ((expr (xpath:compile-xpath v)))
                      (rule (lambda (node result) (declare (ignore result))
                              (xpath:map-node-set
                               #'stp:detach
                               (xpath:evaluate-compiled expr node))))))
                   ("title" (rule (extract-string v 'title)))
                   ("author" (rule (extract-string v 'author)))
                   ("date" (rule (extract-string v 'date)))
                   ("body" (rule (extract v 'body)))
                   ("strip_id_or_class"
                    (rule (let ((expr (xpath:compile-xpath
                                       (fmt "//[@id='~a~:*'] | //[@class='~a']"
                                            v))))
                            (lambda (node result) (declare (ignore result))
                              (xpath:map-node-set
                               #'stp:detach
                               (xpath:evaluate-compiled expr node))))))
                   ("replace_string")))))))

(defun extract/site-config (node conf)
  (lret ((parse (make 'site-parse)))
    (xpath:with-namespaces ((nil xhtml-ns))
      (dolist (rule (rules conf))
        (funcall rule node parse)))))
