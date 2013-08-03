;;;; trie.lisp

(defpackage #:trie
  (:use :cl :cl-ppcre)
  (:export #:*trie*
           #:node
           #:node-eow
           #:insert-string
           #:find-string
           #:build-trie))

(in-package #:trie)

(defparameter *trie* nil)

(defclass node ()
  ((eow
    :initarg :eow
    :initform nil
    :accessor node-eow)
   (children
    :initarg :children
    :initform (make-hash-table :test #'eq)
    :accessor node-children)))

(defmethod insert-char ((n node) char &optional eow)
  (if (and eow (gethash char (node-children n) nil))
      (progn
        (setf (node-eow (gethash char (node-children n))) t)
        (nth-value 0 (gethash char (node-children n))))
      (setf (gethash char (node-children n))
            (make-instance 'node :eow eow))))

(defmethod find-char ((n node) char)
  (gethash char (node-children n) nil))

(defmethod insert-string ((n node) string)
  (let ((length (length string))
        (index 0)
        (current n))
    (loop until (= index (1- length)) do
         (let ((next (find-char current (aref string index))))
           (if next
               (setf index (1+ index)
                     current next)
               (return))))
    (loop until (= index (1- length)) do
         (let ((next (insert-char current (aref string index))))
           (setf index (1+ index)
                 current next)))
    (insert-char current (aref string index) t)))

(defmethod find-string ((n node) string)
  (loop with current = n
     for c across string do
       (setf current (find-char current c))
     finally (return current)))

(defun build-trie (wordlist)
  (declare (ignore wordlist))
  (error "Not implemented"))

