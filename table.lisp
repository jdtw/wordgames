(in-package #:words)

(declaim (optimize (speed 3)
                   (debug 0)
                   (compilation-speed 0)
                   (safety 0)
                   (space 0)))

(defparameter *table* nil)

(defmethod find-string ((store hash-table) string)
  (multiple-value-bind (wordp prefixp)
      (gethash (intern (string-upcase string)) store nil)
    (values prefixp wordp)))

(defmethod build ((table hash-table) path)
  (labels ((prefixes (string)
             (declare (type (simple-base-string) string))
             (loop
                for end from 1 to (1- (length string))
                collect (intern (subseq string 0 end)))))
    (with-open-file (file path :element-type 'base-char)
      (loop
         for line = (read-line file nil :eof)
         until (eq line :eof)
         do (let ((word (scan-to-strings "^([A-Z]{2,15})" line)))
              (loop for sym in (prefixes word)
                 do (when (not (gethash sym table nil))
                      (setf (gethash sym table) nil)))
              (setf (gethash (intern word) table) t)))))
  (setf *table* table))
