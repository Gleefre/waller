(in-package #:waller)

(defun %level-file (n)
  (data-path (format nil (c :level-file-template) n)))

(defun %level-cleared (n)
  (data-path (format nil (c :level-cleared-templay) n)))

(defclass level ()
  ((number :initarg :n :accessor level-number)
   (file   :initarg :file :accessor level-file)
   (board-cache :initarg :board-cache :accessor board-cache :initform NIL)
   (cleared :initarg :cleared :accessor level-cleared :initform NIL)))

(defun make-level (n &optional base-board)
  (make-instance 'level
                 :n n
                 :file (%level-file n)
                 :cleared (probe-file (%level-cleared n))
                 :board-cache base-board))

(defun level-board (level)
  (or (board-cache level)
      (setf (board-cache level)
            (load-board (level-file level)))))

(defun (setf level-board) (board level)
  (save-board board (level-file level)))

(defun level-save-board (level)
  (setf (level-board level)
        (level-board level)))

(defun level-reset (level)
  (when (probe-file (level-file level))
    (setf (board-cache level) NIL)))

(defun level-clear (level)
  (unless (level-cleared level)
    (open (%level-cleared (level-number level))
          :direction :probe
          :if-does-not-exist :create)
    (setf (level-cleared level) T)))

(defun level-unclear (level)
  (when (level-cleared level)
    (setf (level-cleared level) NIL)
    (alexandria:when-let ((flag-file (probe-file (%level-cleared (level-number level)))))
      (delete-file flag-file))))

(defun edit-level (n &optional (width 10) (height 10) new?)
  (editor :level (make-level n (when new? (make-board width height)))))

(defun %get-levels ()
  (loop with result = (make-array 0 :adjustable T :fill-pointer 0)
        for n from 1
        for file = (probe-file (%level-file n))
        while file
        do (vector-push-extend (make-level n) result)
        finally (return result)))

(defparameter *levels* NIL)

(defun get-levels ()
  (or *levels* (setf *levels* (%get-levels))))
