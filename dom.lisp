(in-package :cl-boilerpipe)

(defun node->string (node)
  (let ((sink (cxml:make-string-sink)))
    (stp:serialize node sink)
    (sax:end-document sink)))

(defun count-tags (node)
  (let ((count 0))
    (stp:map-recursively
     (lambda (node)
       (when (typep node 'stp:element)
         (incf count)))
     node)
    count))

(defun node-text-density (node)
  (float (/ (length (node->string node))
            (count-tags node))))

(defun node-composite-text-density (node))

(defun density-sum (node)
  (mapc #'stp:detach (css:query "script, style" node))
  (stp:do-recursively (node node)
    (typecase node
      (stp:comment ))
    )
  )

(defun extract-content (node &key threshold)
  (labels ((extract-content (node)
             (if (> (node-text-density node) threshold)
                 (let ((content (find-max-density-sum-tag n)))
                   (mark-content content)
                   (stp:map-children nil #'extract-content node)))))

    )
  )
