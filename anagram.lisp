;;;; anagram.lisp

(defpackage #:anagram
  (:use :cl :words)
  (:export #:solve))

(in-package #:anagram)

(defun solve (string)
  (declare (ignore string))
  (error "Not implemented"))
