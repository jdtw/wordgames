;;;; boggle.lisp

(defpackage #:boggle
  (:use #:cl #:words)
  (:export #:board
           #:play-boggle
           #:board-print
           #:initialize-words))

(in-package #:boggle)

(declaim (optimize (speed 3)
                   (space 0)
                   (safety 0)
                   (debug 0)
                   (compilation-speed 0)))

(defparameter *store* nil)

(defclass board ()
  ((size
    :initarg :size
    :initform 4
    :reader board-size
    :type fixnum)
   (data
    :initarg :data
    :accessor board-data
    :type simple-string)))

(defmethod initialize-instance :after ((b board) &key generator)
  (let ((board-size (board-size b)))
    (declare (fixnum board-size))
    (assert (< 0 board-size)
            nil
            "Size must be > 0.")
    (if (slot-boundp b 'data)
        (assert (= (length (board-data b)) (* board-size board-size))
                nil
                ":data dimension must be ~d."
                (* board-size board-size))        
        (if (functionp generator)
            (progn
              (setf (board-data b)
                    (make-array (* board-size board-size)
                                :element-type 'base-char))
              (loop for i below board-size do
                   (loop
                      for j below board-size
                      do (setf (board-at b i j) (funcall generator i j)))))
            (setf (board-data b)
                  (make-array (* board-size board-size)
                              :element-type 'base-char)))))
  b)

(defun board-at (b i j)
  (declare (fixnum i j))
  (let ((data (board-data b)))
    (declare (type (simple-string) data))
    (aref data (the fixnum (+ (the fixnum (* i (the fixnum (board-size b)))) j)))))

(defun (setf board-at) (value b i j)
  (declare (fixnum i j))
  (setf (aref (the (simple-string) (board-data b))
              (the fixnum
                   (+ (the fixnum
                           (* i (the fixnum (board-size b))))
                      j)))
        value))

(defun board-print (b)
  (loop for i below (the fixnum (board-size b)) do
       (loop for j below (the fixnum (board-size b)) do
            (format t "~3a" (board-at b i j)))
       (terpri)))

(defun board-moves (b i j &optional seen-before)
  (declare (fixnum i j))
  (remove-if
   (lambda (pos)
     (destructuring-bind (i . j) pos
       (declare (fixnum i j))
       (let ((board-size (board-size b)))
         (declare (fixnum board-size))
         (or (member pos seen-before :test #'equal)
             (< i 0)
             (< j 0)
             (>= j board-size)
             (>= i board-size)))))
   (list (cons (the fixnum (1+ i)) (the fixnum (1+ j)))
         (cons (the fixnum (1+ i)) j)
         (cons (the fixnum (1+ i)) (the fixnum (1- j)))
         (cons i (the fixnum (1+ j)))
         (cons i (the fixnum (1- j)))
         (cons (the fixnum (1- i)) (the fixnum (1+ j)))
         (cons (the fixnum (1- i)) j)
         (cons (the fixnum (1- i)) (the fixnum (1- j))))))

(defun initialize-words (path &optional (type :table))
  (setf *store* (initialize path type)))

(defun play-boggle (board)
  (labels ((move (pos dirty current-word seen-before next-moves past-moves words)
             "Recursive backtracking implementation that finds words
              starting at 'pos'"
             (vector-push-extend (board-at board (car pos) (cdr pos)) current-word)
             (multiple-value-bind (prefixp wordp) (find-string *store* current-word)
               (when (and (not dirty) wordp)
                 (push
                  (make-array (length current-word)
                              :element-type 'standard-char
                              :initial-contents current-word)
                  words))
               (if (or (null next-moves) (not prefixp))
                   ;; backtrack if it's not a prefix or we're out of moves.
                   (if (null past-moves)
                       words ;; nowhere to backtrack to. We're done.
                       (progn
                         (vector-pop current-word)
                         (vector-pop current-word)
                         (move (caar past-moves)
                               t
                               current-word
                               (cdr seen-before)
                               (cdar past-moves)
                               (cdr past-moves)
                               words)))
                   (let* ((next (car next-moves))
                          (next-seen (cons pos seen-before))
                          (next-next-moves (board-moves board
                                                        (car next)
                                                        (cdr next)
                                                        next-seen)))
                     (move next
                           nil
                           current-word
                           next-seen
                           next-next-moves
                           (cons (cons pos (cdr next-moves)) past-moves)
                           words))))))
    (board-print board)
    (terpri)
    (let ((word-list (remove-duplicates
                      (loop for i below (the fixnum (board-size board)) append
                           (loop for j below (the fixnum (board-size board)) append
                                (move (cons i j)
                                      nil
                                      (make-array 10
                                                  :element-type 'base-char
                                                  :adjustable t
                                                  :fill-pointer 0)
                                      nil
                                      (board-moves board i j)
                                      nil
                                      nil)))
                      :test #'equal)))
      (declare (cons word-list))
      (setf word-list (sort word-list #'> :key #'length))
      (format t "~{~{~10a~^ ~}~^~%~}"
              (loop for i below (length word-list) by 5
                 collect (subseq word-list i (min (length word-list) (+ i 5))))))))



