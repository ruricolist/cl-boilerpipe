(in-package #:cl-boilerpipe)

(defparameter *empty* (stp:make-text ""))

;; Default extractor: simple-block-fusion, block-proximity-fusion,
;; density-rules-classifier.

(defun strip-boilerplate (text)
  (let* ((document (chtml:parse text (stp:make-builder)))
         (text-blocks (document->text-blocks document))
         (ok (loop for prev = *empty* then curr
                   for (curr . rest) on text-blocks
                   for next = (if rest (first rest) *empty*)
                   for class = (classify prev curr next)
                   if (eql class :content)
                     collect curr))
         (new (let ((root (stp:make-element "div")))
                (dolist (tb ok)
                  (stp:append-child root tb))
                (stp:make-document root))))
    (stp:serialize new (cxml:make-string-sink))))

(defclass text-block ()
  ((offset :initarg :offset)
   (nodes :initarg :nodes :accessor text-block-nodes)
   text-density link-density)
  (:default-initargs :nodes nil))

(defmethod initialize-instance :after ((self text-block) &key nodes)
  (with-slots (text-density link-density) self
    (setf text-density (compute-text-density nodes)
          link-density (compute-link-density nodes))))

(defun add-node (text-block node)
  (with-slots (nodes) text-block
    (setf nodes (nconc nodes (list node)))))

(defun merge-text-blocks (block1 block2)
  (with-slots ((pos1 offset) (blocks1 blocks)) block1
    (with-slots ((pos2 offset) (blocks2 blocks)) block2
      (make-instance 'text-block
                     :offset (max pos1 pos2)
                     :nodes (nconc blocks1 block2)))))

(defun document->text-blocks (document)
  (let* ((text-blocks '()))

    (labels ((find-text-blocks (node)
               (let ((children (stp:list-children node)))
                 (if (or (heading? node)
                         (every (lambda (node)
                                  (typecase node
                                    (stp:text t)
                                    (stp:element
                                     (inline? (stp:local-name node)))))
                                children))
                     (push (prog1 node (stp:detach node)) text-blocks)
                     (mapc #'find-text-blocks children)))))
      (mapc #'find-text-blocks (stp:list-children (car (css:query "body" document)))))

    (proximity-fusion (simple-fusion (nreverse text-blocks)))))

(defun delete-blanks (blocks)
  (delete-if (lambda (item)
               (and (typep item 'stp:text)
                    (every #'whitespacep (stp:data item))))
             blocks))

(defun simple-fusion (blocks)
  (reduce
   (lambda (x y)
     (cond ((not y)
            (cons x y))
           ;; TODO block proximity fusion
           ((= (text-density x)
               (text-density (car y)))
            (cons (fuse-blocks x (car y))
                  (cdr y)))
           (t (cons x y))))
   blocks
   :initial-value nil
   :from-end t))

(defun proximity-fusion (blocks)
  (reduce
   (lambda (x ys)
     (if (not ys)
         (cons x ys)
         (destructuring-bind (y . ys) ys
           (cond ((or (and (typep x 'stp:text)
                           (typep y 'stp:text)))
                  (cons (stp:make-text
                         (format nil "~a ~a"
                                 (stp:data x)
                                 (stp:data y)))
                        ys))
                 ((and (or (typep x 'stp:text)
                           (inline? (stp:local-name x)))
                       (or (typep y 'stp:text)
                           (inline? (stp:local-name y))))
                  (cons (fuse-blocks x y) ys))
                 (t (list* x y ys))))))
   blocks
   :from-end t
   :initial-value nil))

(defun fuse-blocks (x y)
  (make-text-block
   (append (fusibles x)
           (fusibles y))))

(defun make-text-block (elements)
  (let ((tb (stp:make-element "span")))
    (setf (stp:attribute-value tb "class") "text-block")
    (dolist (e elements)
      (stp:detach e)
      (stp:append-child tb e))
    tb))

(defun fusibles (element)
  (if (text-block? element)
      (stp:list-children element)
      (list element)))

(defun text-block? (element)
  (and (typep element 'stp:element)
       (equal (stp:local-name element) "span")
       (equal (stp:attribute-value element "class") "text-block")))

(defun inline? (lname)
  (find lname
        '("a" "strike" "u" "b" "i" "em" "strong" "span" "sup" "code" "tt" "sub" "var"
          "abbr" "acronym" "font")
        :test 'equalp))

(defun heading? (lname)
  (find lname '("li" "h1" "h2" "h3")
        :test 'equal))

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

(defun link-density (node)
  (if (typep node 'stp:text)
      0
      (let ((len (text-length node)))
        (if (zerop len)
            0
            (let ((anchors (css:query "a" node)))
              (float
               (/ (reduce #'+ anchors :key #'text-length) len)))))))

(defun text-length (node)
  (typecase node
    (string (length node))
    (stp:text (length (stp:data node)))
    ((or stp:element stp:document)
     (reduce #'+ (stp:list-children node) :key #'text-length))
    (t 0)))

(defun text-density (node &key (width 80))
  (let ((col 0)
        (lines -1)
        (words 0))
    ;; A restrictive idea of words.
    (flet ((compute-density (text)
             (ppcre:do-scans (start end rs re "[a-zA-Z0-9]+" text)
               (incf words)
               (let ((len (- end start)))
                 (when (> (incf col len) width)
                   (incf lines 1)
                   (setf col len))))))
      (typecase node
        (stp:text (compute-density (stp:data node)))
        (stp:element
         (stp:do-recursively (n node)
           (when (typep n 'stp:text)
             (compute-density (stp:data n)))))))

    (float (/ words (max 1 lines)))))
