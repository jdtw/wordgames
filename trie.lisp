;;;; trie.lisp

(in-package #:words)

(declaim (optimize (speed 3)
                   (debug 0)
                   (compilation-speed 0)
                   (safety 0)
                   (space 0)))

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
  (declare (base-char char))
  (symbol-macrolet ((child (gethash char (node-children n) nil)))
    (if child
        (when eow
          (setf (node-eow child) t)
          (nth-value 0 child))
        (progn
          (setf child (make-instance 'node :eow eow))))))

(defmethod find-char ((n node) char)
  (declare (base-char char))
  (gethash char (node-children n) nil))

(defmethod insert-string ((n node) string)
  (declare (type (simple-array) string))
  (let* ((length (length string))
         (string (make-array length
                             :element-type 'base-char
                             :initial-contents string))
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
  (declare (type (simple-array) string))
  (let ((string (make-array (length string)
                            :element-type 'base-char
                            :initial-contents string)))
    (declare (type (simple-base-string) string))
    (loop with current = n
       for c across string do
         (setf current (find-char current c))
       finally (return (if current
                           (values t (node-eow current))                         
                           (values nil nil))))))

(defmethod  build ((trie node) path)
  (with-open-file (file path :element-type 'base-char)
    (loop
       for line = (read-line file nil :eof)
       until (eq line :eof)
       do (let ((word (scan-to-strings "^([A-Z]{2,15})" line)))
            (insert-string trie (string-downcase word)))))
  (setf *trie* trie))
