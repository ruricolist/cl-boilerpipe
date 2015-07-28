(in-package #:cl-boilerpipe)

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

(defun wc (text)
  (max 0 (floor (count-matches "\\b" text) 2)))
