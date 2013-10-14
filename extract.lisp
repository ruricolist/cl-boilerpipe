(in-package #:cl-boilerpipe)

(defclass text-block ()
  ((offset :initarg :offset :accessor offset)
   (nodes :initarg :nodes :accessor nodes)
   (text-density :accessor text-density)
   (link-density :accessor link-density))
  (:default-initargs :nodes nil))

(defmethod print-object ((self text-block) stream)
  (with-slots (offset text-density link-density) self
    (print-unreadable-object (self stream :type t)
      (format stream "~d td ~d ld ~d" offset text-density link-density))))

(defun count-matches (re text &key start end)
  (let ((sum 0))
    (ppcre:do-matches (s e re text nil :start start :end end)
      (incf sum))
    sum))

(define-compiler-macro count-matches (&whole decline
                                             &environment env
                                             re text &key start end)
  (if (constantp re env)
      `(count-matches (load-time-value (ppcre:create-scanner ,re)) ,text
                      ,@(when start `(:start ,start))
                      ,@(when end `(:end ,end)))
      decline))

(defun strip-boilerplate (html)
  (-> html
      (chtml:parse (stp:make-builder))
      document->text-blocks
      filter-blocks
      blocks->document
      (stp:serialize (cxml:make-string-sink))))

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
      (let ((para (stp:make-element "p")))
        (setf (stp:attribute-value para "class") "text-block")
        (with-slots (nodes) tb
          (dolist (node nodes)
            (stp:append-child para node)))
        (stp:append-child root para)))
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

(defun wc (text)
  (max 0 (floor (count-matches "\\b" text) 2)))

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

(defun document->text-blocks (document)
  (let ((text-blocks '())
        (offset -1)
        (nodes '()))
    (flet ((flush ()
             (push (make-instance 'text-block
                                  :offset (incf offset)
                                  :nodes (nreverse (shiftf nodes nil)))
                   text-blocks))
           (collect (node)
             (stp:detach node)
             (push node nodes)))
      (declare (dynamic-extent (function flush)
                               (function collect)))
      (labels ((rec (node)
                 (let ((children (stp:list-children node)))
                   (dolist (node children)
                     (typecase node
                       (stp:text
                        (let ((data (stp:data node)))
                          (when (and (> (length data) 1)
                                     (notevery #'whitespacep data))
                            (collect node))))
                       (stp:element
                        (unless (search "comments" (stp:attribute-value node "class"))
                          (let ((class (classify-tag (stp:local-name node))))
                            (case class
                              (:ignorable)
                              (:inline
                               (when (stp:list-children node)
                                 (collect node)))
                              (:keep (collect node))
                              (otherwise
                               (flush)
                               (rec node)))))))))))
        (rec (car (css:query "body" document)))))
    (nreverse (delete-if (lambda (tb) (zerop (text-density tb))) text-blocks))))

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
    (("style" "script" "option" "object" "embed" "applet" "link")
     :ignorable)
    (("li" "h1" "h2" "h3")
     :heading)
    (("a" "strike" "u" "b" "i" "em" "strong" "span" "sup" "code" "tt" "sub" "var"
          "abbr" "acronym" "font"
          "mark" "cite" "del" "ins" "kbd" "q"
          "s" "samp")
     :inline)
    ("img" :keep)))

(defun classify (prev curr next)
  (let ((curr-link-density (link-density curr))
        (curr-text-density (text-density curr))
        (prev-text-density (text-density prev))
        (prev-link-density (link-density prev))
        (next-text-density (text-density next)))
    (if (<= curr-link-density 0.333333)
        (if (<= prev-link-density 0.555555)
            (if (<= curr-text-density 9)
                (if (<= next-text-density 10)
                    (if (<= prev-text-density 4)
                        :boilerplate
                        :content))
                (if (zerop next-text-density)
                    :boilerplate
                    :content))
            (if (<= next-text-density 11)
                :boilerplate
                :content))
        :boilerplate)))

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
    ;; A restrictive idea of words.
    (labels ((process-text (text)
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
