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
    :initform (make-hash-table :test #'eq :size 26)
    :accessor node-children)))

(defmethod insert-char ((n node) char &optional eow)
  (symbol-macrolet ((child (gethash char (node-children n) nil)))
    (if child
        (when eow
          (setf (node-eow child) t)
          (nth-value 0 child))
        (setf child (make-instance 'node :eow eow)))))

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
  (setf *trie* (make-instance 'node))
  (with-open-file (file wordlist :element-type 'base-char)
    (loop
       for line = (read-line file nil :eof)
       until (eq line :eof)
       do (let ((word (scan-to-strings "^([A-Z]{2,15})" line)))
            (insert-string *trie* (string-downcase word))))))
