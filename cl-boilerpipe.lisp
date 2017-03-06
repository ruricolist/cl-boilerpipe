;;;; cl-boilerpipe.lisp

(in-package #:cl-boilerpipe)

;;; "cl-boilerpipe" goes here. Hacks and glory await!

(deftype tag-class ()
  '(member :unknown :ignorable :heading :inline :blockquote :list :pre :keep))

(deftype node-class ()
  '(member :boilerplate :content))

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
      html5-parser:parse-html5
      (fxml.html5:serialize-dom (fxml.stp:make-builder))
      strip-boilerplate/raw))

(defmethod strip-boilerplate/raw ((html fxml.stp:document))
  (~> html
      document->text-blocks
      filter-blocks
      blocks->document))

(defmethod strip-boilerplate/raw ((html stp:document))
  (strip-boilerplate/raw
   (stp:serialize html (fxml.stp:make-builder))))

(defun strip-boilerplate (html)
  (fxml.stp:serialize (strip-boilerplate/raw html) (fxml:make-string-sink)))

(defun filter-blocks (text-blocks)
  (let ((empty (make-instance 'text-block :offset -1)))
    (loop for prev = empty then curr
          for (curr . rest) on text-blocks
          for next = (if rest (first rest) empty)
          when (content? prev curr next)
            collect curr)))

(defun blocks->document (text-blocks)
  (let ((root (fxml.stp:make-element "div")))
    (dolist (tb text-blocks)
      (let ((elt (fxml.stp:make-element (tag tb))))
        (setf (fxml.stp:attribute-value elt "class") "text-block")
        (with-slots (nodes) tb
          (dolist (node nodes)
            (fxml.stp:append-child elt node)))
        (fxml.stp:append-child root elt)))
    (fxml.stp:make-document root)))

(define-do-macro do-text-block-nodes ((node text-block &optional ret) &body body)
  `(map-text-block-nodes
    (lambda (,node)
      ,@body)
    ,text-block))

(defun text-block-wc (text-block)
  (summing
    (do-text-block-nodes (node text-block)
      (when (typep node 'fxml.stp:text)
        (let ((data (fxml.stp:data node)))
          (sum (wc data)))))))

(defun text-block-contains-any-p (text-block strings)
  (do-text-block-nodes (node text-block)
    (and (typep node 'fxml.stp:text)
         (let ((data (fxml.stp:data node)))
           (some (lambda (string)
                   (search (string string) data :test #'char-equal))
                 strings))
         (return t))))

(defun text-block-text (text-block)
  (with-output-to-string (s)
    (do-text-block-nodes (node text-block)
      (when (typep node 'fxml.stp:text)
        (write-string (fxml.stp:data node) s)
        (write-char #\Space s)))))

(defun map-text-block-nodes (fn tb)
  (with-slots (nodes) tb
    (dolist (node nodes)
      (fxml.stp:do-recursively (node node)
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

(defun document->text-blocks (document &aux (*tag* "p"))
  (local
    (def text-blocks '())
    (def offset -1)
    (def nodes '())

    (defun flush ()
      (push (make-instance 'text-block
                           :tag *tag*
                           :offset (incf offset)
                           :nodes (nreverse (shiftf nodes nil)))
            text-blocks))
    
    (defun collect (node)
      (fxml.stp:detach node)
      (push node nodes))

    (defun junk? (node)
      (let ((class (fxml.stp:attribute-value node "class"))
            (id (fxml.stp:attribute-value node "id")))
        (or (string*= "comment" id)
            (ppcre:scan "comment|illegalstyle|hidden" class))))

    (defun rec (node)
      (dolist (node (fxml.stp:list-children node))
        (typecase node
          (fxml.stp:text
           (let ((data (fxml.stp:data node)))
             (when (and (> (length data) 1)
                        (notevery #'whitespacep data))
               (collect node))))
          (fxml.stp:element
           (unless (junk? node)
             (let ((tag (fxml.stp:local-name node)))
               (ecase-of tag-class (classify-tag tag)
                 (:ignorable)
                 (:keep (collect node))
                 (:inline
                  (when (fxml.stp:list-children node)
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
                 ((:unknown :list)
                  (flush)
                  (rec node)))))))))

    (rec (query1 "body" document))

    (nreverse
     (delete-if (lambda (text-block)
                  (or (zerop (text-density text-block))
                      (null (nodes text-block))))
                text-blocks))))

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
    (("img" "code" "br") :keep)
    (t :unknown)))

(defun classify (prev curr next)
  (if (<= (link-density curr) 0.333333)
      (if (<= (link-density prev) 0.555555)
          (if (<= (text-density curr) 9)
              (if (<= (text-density next) 10)
                  (if (<= (text-density prev) 4)
                      :boilerplate
                      :content)
                  :content)
              (if (zerop (text-density next))
                  :boilerplate
                  :content))
          (if (<= (text-density next) 11)
              :boilerplate
              :content))
      :boilerplate))

(defun content? (prev curr next)
  (ecase-of node-class (classify prev curr next)
    (:boilerplate nil)
    (:content t)))

(defun boilerplate? (prev curr next)
  (ecase-of node-class (classify prev curr next)
    (:boilerplate t)
    (:content nil)))

(defun compute-link-density (nodes)
  (let ((len 0)
        (anchor-len 0))

    (dolist (node nodes)
      (incf len (text-length node))
      (unless (typep node 'fxml.stp:text)
        (incf anchor-len (reduce #'+ (css:query "a" node) :key #'text-length))))

    (if (zerop len)
        0
        (/ anchor-len (float len)))))

(defun text-length (node)
  (typecase node
    (string (length node))
    (fxml.stp:text (length (fxml.stp:data node)))
    ((or fxml.stp:element fxml.stp:document)
     (reduce #'+ (fxml.stp:list-children node) :key #'text-length))
    (t 0)))

(defun compute-text-density (nodes &key (width 80))
  (local
    (def col 0)
    (def lines -1)
    (def words 0)

    (defun process-text (text)
      ;; A restrictive idea of words.
      (ppcre:do-scans (start end rs re "[a-zA-Z0-9]+" text)
        (incf words)
        (let ((len (- end start)))
          (when (> (incf col len) width)
            (incf lines 1)
            (setf col len)))))

    (defun process-node (node)
      (typecase node
        (fxml.stp:text (process-text (fxml.stp:data node)))
        (fxml.stp:element
         (fxml.stp:do-recursively (n node)
           (when (typep n 'fxml.stp:text)
             (process-text (fxml.stp:data n)))))))

    (dolist (node nodes)
      (process-node node))

    (/ words (float (max 1 lines)))))
