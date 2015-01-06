;;;; cl-boilerpipe.lisp

(in-package #:cl-boilerpipe)

;;; "cl-boilerpipe" goes here. Hacks and glory await!

(defclass text-block ()
  ((offset :initarg :offset :accessor offset)
   (nodes :initarg :nodes :accessor nodes)
   (text-density :accessor text-density)
   (link-density :accessor link-density)
   (tag :initarg :tag :accessor tag))
  (:default-initargs :nodes nil :tag "p"))

(defmethod print-object ((self text-block) stream)
  (with-slots (offset tag text-density link-density) self
    (print-unreadable-object (self stream :type t)
      (format stream "~d ~a ~,2f ld ~,2f"
              offset tag text-density link-density))))

(defmethod strip-boilerplate/raw ((html string))
  (~> html
      (html5-parser:parse-html5)
      (html5-sax:serialize-dom (stp:make-builder))
      strip-boilerplate/raw))

(defmethod strip-boilerplate/raw ((html stp:document))
  (~> html
      document->text-blocks
      filter-blocks
      blocks->document))

(defun strip-boilerplate (html)
  (stp:serialize (strip-boilerplate/raw html) (cxml:make-string-sink)))

(defun filter-blocks (text-blocks)
  (let ((empty (make-instance 'text-block :offset -1)))
    (loop for prev = empty then curr
          for (curr . rest) on text-blocks
          for next = (if rest (first rest) empty)
          for class = (classify prev curr next)
          if (eql class :content)
            collect curr)))

(defun blocks->document (text-blocks)
  (let ((root (stp:make-element "div")))
    (dolist (tb text-blocks)
      (let ((elt (stp:make-element (tag tb))))
        (setf (stp:attribute-value elt "class") "text-block")
        (with-slots (nodes) tb
          (dolist (node nodes)
            (stp:append-child elt node)))
        (stp:append-child root elt)))
    (stp:make-document root)))

(defun text-block-wc (text-block)
  (let ((sum 0))
    (map-text-block-nodes
     (lambda (node)
       (when (typep node 'stp:text)
         (let ((data (stp:data node)))
           (incf sum (wc data)))))
     text-block)
    sum))

(defun text-block-contains-any-p (text-block strings)
  (block nil
    (map-text-block-nodes
     (lambda (node)
       (and (typep node 'stp:text)
            (let ((data (stp:data node)))
              (some (lambda (string)
                      (search (string string) data :test #'char-equal))
                    strings))
            (return t)))
     text-block)))

(defun text-block-text (text-block)
  (with-output-to-string (s)
    (map-text-block-nodes
     (lambda (node)
       (when (typep node 'stp:text)
         (write-string (stp:data node) s)
         (write-char #\Space s)))
     text-block)))

(defun map-text-block-nodes (fn tb)
  (with-slots (nodes) tb
    (dolist (node nodes)
      (stp:do-recursively (node node)
        (funcall fn node)))))

(defmethod initialize-instance :after ((self text-block) &key nodes)
  (with-slots (text-density link-density) self
    (setf text-density (compute-text-density nodes)
          link-density (compute-link-density nodes))))

(defun add-node (text-block node)
  (with-slots (nodes) text-block
    (setf nodes (nconc nodes (list node)))))

(defun merge-text-blocks (block1 block2)
  (with-slots ((pos1 offset) (nodes1 nodes)) block1
    (with-slots ((pos2 offset) (nodes2 nodes)) block2
      (make-instance 'text-block
                     :offset (max pos1 pos2)
                     :nodes (nconc nodes1 nodes2)))))

(defvar *tag*)

(defun document->text-blocks (document)
  (let ((text-blocks '())
        (offset -1)
        (nodes '())
        (*tag* "p"))
    (flet ((flush ()
             (push (make-instance 'text-block
                                  :tag *tag*
                                  :offset (incf offset)
                                  :nodes (nreverse (shiftf nodes nil)))
                   text-blocks))
           (collect (node)
             (stp:detach node)
             (push node nodes)))
      (labels ((rec (node)
                 (dolist (node (stp:list-children node))
                   (typecase node
                     (stp:text
                      (let ((data (stp:data node)))
                        (when (and (> (length data) 1)
                                   (notevery #'whitespacep data))
                          (collect node))))
                     (stp:element
                      (unless (let ((class (stp:attribute-value node "class"))
                                    (id (stp:attribute-value node "id")))
                                (or (string*= "comment" id)
                                    (ppcre:scan "comment|illegalstyle|hidden" class)))
                        (let* ((tag (stp:local-name node))
                               (class (classify-tag tag)))
                          (case class
                            (:ignorable)
                            (:keep (collect node))
                            (:inline
                             (when (stp:list-children node)
                               (collect node)))
                            ((:heading :pre)
                             (flush)
                             (let ((*tag* tag))
                               (rec node)
                               (flush)))
                            (:blockquote
                              (flush)
                              (let ((*tag* "blockquote"))
                                (rec node)
                                (flush)))
                            (otherwise
                             (flush)
                             (rec node))))))))))
        (rec (query1 "body" document))))
    (nreverse
     (delete-if (lambda (text-block)
                  (or (zerop (text-density text-block))
                      (null (nodes text-block))))
                text-blocks))))

(defun simple-block-fusion (blocks)
  (reduce
   (lambda (x ys)
     (match ys
       ((list* y ys)
        (if (= (text-density x)
               (text-density y))
            (cons (merge-text-blocks x y) ys)
            (list* x y ys)))
       (otherwise (cons x ys))))
   blocks
   :initial-value nil
   :from-end t))

(defun block-proximity-fusion (blocks)
  (reduce
   (lambda (x ys)
     (match ys
       ((list* y ys)
        (if (= (1+ (offset x)) (offset y))
            (cons (merge-text-blocks x y) ys)
            (list* x y ys)))))
   blocks
   :from-end t
   :initial-value nil))

(defun classify-tag (lname)
  (string-case lname
    (("style" "script" "noscript" "option" "object" "embed" "applet" "link" "form")
     :ignorable)
    (("h1" "h2" "h3" "h4" "h5" "h6")
     :heading)
    (("a" "strike" "u" "b" "i" "em" "strong" "span" "sup" "tt" "sub" "var"
          "abbr" "acronym" "font"
          "mark" "cite" "del" "ins" "kbd" "q"
          "s" "samp")
     :inline)
    ("blockquote" :blockquote)
    (("ul" "ol") :list)
    ("pre" :pre)
    (("img" "code" "br") :keep)))

(defun classify (prev curr next)
  (if (<= (link-density curr) 0.333333)
      (if (<= (link-density prev) 0.555555)
          (if (<= (text-density curr) 9)
              (if (<= (text-density next) 10)
                  (if (<= (text-density prev) 4)
                      :boilerplate
                      :content))
              (if (zerop (text-density next))
                  :boilerplate
                  :content))
          (if (<= (text-density next) 11)
              :boilerplate
              :content))
      :boilerplate))

(defun compute-link-density (nodes)
  (let ((len 0)
        (anchor-len 0))

    (dolist (node nodes)
      (incf len (text-length node))
      (unless (typep node 'stp:text)
        (incf anchor-len (reduce #'+ (css:query "a" node) :key #'text-length))))

    (if (zerop len)
        0
        (/ anchor-len (float len)))))

(defun text-length (node)
  (typecase node
    (string (length node))
    (stp:text (length (stp:data node)))
    ((or stp:element stp:document)
     (reduce #'+ (stp:list-children node) :key #'text-length))
    (t 0)))

(defun compute-text-density (nodes &key (width 80))
  (let ((col 0)
        (lines -1)
        (words 0))
    (labels ((process-text (text)
               ;; A restrictive idea of words.
               (ppcre:do-scans (start end rs re "[a-zA-Z0-9]+" text)
                 (incf words)
                 (let ((len (- end start)))
                   (when (> (incf col len) width)
                     (incf lines 1)
                     (setf col len)))))
             (process-node (node)
               (typecase node
                 (stp:text (process-text (stp:data node)))
                 (stp:element
                  (stp:do-recursively (n node)
                    (when (typep n 'stp:text)
                      (process-text (stp:data n))))))))

      (dolist (node nodes)
        (process-node node)))

    (/ words (float (max 1 lines)))))
