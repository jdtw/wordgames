;;;; boggle.lisp

(defpackage #:boggle
  (:use #:cl #:words)
  (:export #:board
           #:play-boggle
           #:board-print))

(in-package #:boggle)

(declaim (optimize (speed 3)
                   (space 0)
                   (safety 0)
                   (debug 0)
                   (compilation-speed 0)))

(defclass board ()
  ((size
    :initarg :size
    :initform 4
    :reader board-size)
   (data
    :initarg :data
    :accessor board-data)))

(defmethod initialize-instance :after ((b board) &key generator)
  (let ((board-size (board-size b)))
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
                                :element-type 'standard-char))
              (loop for i below board-size do
                   (loop
                      for j below board-size
                      do (setf (board-at b i j) (funcall generator i j)))))
            (setf (board-data b)
                  (make-array (* board-size board-size)
                              :element-type 'standard-char
                              :initial-element #\Nul)))))
  b)

(defun board-at (b i j)
  (aref (board-data b) (+ (* i (board-size b)) j)))

(defun (setf board-at) (value b i j)
  (setf (aref (board-data b) (+ (* i (board-size b)) j)) value))

(defun board-print (b)
  (loop for i below (board-size b) do
       (loop for j below (board-size b) do
            (format t "~3a" (board-at b i j)))
       (terpri)))

(defun board-moves (b i j &optional seen-before)
  (remove-if
   (lambda (pos)
     (destructuring-bind (i . j) pos
       (or (member pos seen-before :test #'equal)
           (< i 0)
           (< j 0)
           (>= j (board-size b))
           (>= i (board-size b)))))
   (list (cons (1+ i) (1+ j))
         (cons (1+ i) j)
         (cons (1+ i) (1- j))
         (cons i (1+ j))
         (cons i (1- j))
         (cons (1- i) (1+ j))
         (cons (1- i) j)
         (cons (1- i) (1- j)))))

(defun play-boggle (board)
  (labels ((move (pos current-word seen-before next-moves past-moves words)
             "Recursive backtracking implementation that finds words
              starting at 'pos'"
             (push (board-at board (car pos) (cdr pos)) current-word)
             (let ((word (coerce (reverse current-word) 'string)))
               (when (word-p word)
                 (push word words))
               (if (or (not (prefix-p word)) (null next-moves))
                   ;; backtrack if it's not a prefix or we're out of moves.
                   (if (null past-moves)
                       words ;; nowhere to backtrack to. We're done.
                       (move (caar past-moves)
                             (cddr current-word)
                             (cdr seen-before)
                             (cdar past-moves)
                             (cdr past-moves)
                             words))
                   (let* ((next (car next-moves))
                          (next-seen (cons pos seen-before))
                          (next-next-moves (board-moves board
                                                        (car next)
                                                        (cdr next)
                                                        next-seen)))
                     (move next
                           current-word
                           next-seen
                           next-next-moves
                           (cons (cons pos (cdr next-moves)) past-moves)
                           words))))))
    (board-print board)
    (terpri)
    (let ((word-list (sort 
                      (remove-duplicates 
                       (loop for i below (board-size board) append
                            (loop for j below (board-size board) append
                                 (move (cons i j) nil nil (board-moves board i j) nil nil)))
                       :test #'equal)
                      #'> :key #'length)))
      (format t "~{~{~10a~^ ~}~^~%~}"
              (loop for i below (length word-list) by 5
                 collect (subseq word-list i (min (length word-list) (+ i 5))))))))
