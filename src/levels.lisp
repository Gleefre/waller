(in-package #:waller)

(defun level-file (n)
  (data-path (format nil "levels/~a.brd" n)))

(defun load-level (n)
  (load-board (level-file n)))

(defun edit-level (n &optional (width 10) (height 10) noload)
  (with-board ((make-board width height))
    (print *board*)
    (editor :board *board* :file (level-file n) :noload noload)))

(defun get-levels ()
  (loop with result = (make-array 0 :adjustable T :fill-pointer 0)
        for n from 1
        while (probe-file (level-file n))
        do (vector-push-extend (load-level n) result)
        finally (return result)))
