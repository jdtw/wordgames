;;;; words.lisp

(defpackage #:words
  (:use :cl :cl-ppcre)
  (:export #:build-dicts
           #:word-p
           #:prefix-p
           #:get-letter))

(in-package #:words)

(defparameter *prefix-dict* nil)
(defparameter *word-dict* nil)

(defun word-p (string)
  (gethash (intern (string-upcase string)) *word-dict* nil))

(defun prefix-p (string)
  (gethash (intern (string-upcase string)) *prefix-dict* nil))

(defun build-dicts (path)
  (setf *prefix-dict* (make-hash-table)
        *word-dict* (make-hash-table))
  (labels ((prefixes (string)
             (loop
                for end from 1 to (1- (length string))
                collect (intern (subseq string 0 end)))))
    (with-open-file (ospd4 path :element-type 'base-char)
      (loop
         for line = (read-line ospd4 nil :eof)
         until (eq line :eof)
         do (let ((word (scan-to-strings "^([A-Z]{2,15})" line)))
              (loop for sym in (prefixes word)
                 do (setf (gethash sym *prefix-dict*) t))
              (setf (gethash (intern word) *word-dict*) t)))))
  (terpri))

(let ((freq-list '((#\a . 8.167)	
                   (#\b . 1.492)	
                   (#\c . 2.782)	
                   (#\d . 4.253)	
                   (#\e . 12.702)	
                   (#\f . 2.228)	
                   (#\g . 2.015)	
                   (#\h . 6.094)	
                   (#\i . 6.966)	
                   (#\j . 0.153)	
                   (#\k . 0.772)	
                   (#\l . 4.025)	
                   (#\m . 2.406)	
                   (#\n . 6.749)	
                   (#\o . 7.507)	
                   (#\p . 1.929)	
                   (#\q . 0.095)	
                   (#\r . 5.987)	
                   (#\s . 6.327)	
                   (#\t . 9.056)	
                   (#\u . 2.758)	
                   (#\v . 0.978)	
                   (#\w . 2.360)	
                   (#\x . 0.150)	
                   (#\y . 1.974)	
                   (#\z . 0.074))))
  (defun get-letter ()
    (loop
       with random = (random 100.0)
       with floor = 0
       for (c . freq) in freq-list
       do (let ((ceiling (+ floor freq)))
            (when (and (>= random floor)
                       (<= random ceiling))
              (return c))
            (setf floor ceiling)))))
