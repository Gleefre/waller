(in-package #:waller)

(defclass board ()
  ((cells     :initarg :cells     :accessor cells)
   (states    :initarg :states    :accessor states
              :initform (make-hash-table :test 'equal))
   (positions :initarg :positions :accessor positions
              :initform (make-hash-table :test 'equal))))

(defun make-board (width height)
  (make-instance 'board :cells (make-array (list width height) :initial-element ())))

;; We have one global board

(defvar *board*)

(defmacro with-board ((board) &body body)
  `(let ((*board* ,board))
     ,@body))

(defun board-width ()
  (array-dimension (cells *board*) 0))

(defun board-height ()
  (array-dimension (cells *board*) 1))

(defun cell (x y)
  (aref (cells *board*) x y))

(defun (setf cell) (new-cell x y)
  (setf (aref (cells *board*) x y) new-cell))

(defmacro do-cells ((&key (cell (gensym "cell")) (x (gensym "x")) (y (gensym "y")) result (board '*board*))
                    &body body)
  `(with-board (,board)
     (dotimes (,x (board-width) ,result)
       (dotimes (,y (board-height))
         (let ((,cell (cell ,x ,y)))
           (declare (ignorable ,cell))
           ,@body)))))

(defun in-board (x y)
  (and (integerp x)
       (integerp y)
       (<= 0 x (1- (board-width)))
       (<= 0 y (1- (board-height)))))

(defun tilep$ (cell &optional (feature :tile))
  (getf cell feature))

(defun tilep (x y &optional (feature :tile))
  (when (in-board x y)
    (tilep$ (cell x y) feature)))

(defmacro tile-add$ (cell feature &optional (state T))
  `(setf (getf ,cell ,feature) ,state))

(defun tile-add (x y feature &optional (state T))
  (when (in-board x y)
    (tile-add$ (cell x y) feature state)))

(defmacro tile-add-new$ (cell feature &optional (state T)
                         &aux (!cell (gensym "cell"))
                              (!feature (gensym "feature")))
  `(let ((,!cell ,cell)
         (,!feature ,feature))
     (unless (tilep$ ,!cell ,!feature)
       (tile-add$ ,!cell ,!feature ,state)
       (setf ,cell ,!cell))))

(defun tile-add-new (x y feature &optional (state T))
  (tile-add-new$ (cell x y) feature state))

(defmacro tile-rem$ (cell feature
                     &aux (!cell (gensym "cell"))
                          (!feature (gensym "feature")))
  `(let ((,!cell ,cell)
         (,!feature ,feature))
     (when (tilep$ ,!cell ,!feature)
       (setf (getf ,!cell ,!feature) NIL)
       (setf ,cell ,!cell))))

(defun tile-rem (x y feature)
  (tile-rem$ (cell x y) feature))

(defmacro tile-toggle$ (cell feature &optional (state T)
                        &aux (!cell (gensym "cell"))
                             (!feature (gensym "feature")))
  `(let ((,!cell ,cell)
         (,!feature ,feature))
     (if (tilep$ ,!cell ,!feature)
         (tile-add$ ,cell ,!feature NIL)
         (tile-add$ ,cell ,!feature ,state))))

(defun tile-toggle (x y feature &optional (state T))
  (tile-toggle$ (cell x y) feature state))

(defun thing-coordinates (name)
  (destructuring-bind (&optional x y) (gethash name (positions *board*))
    (values x y)))

(defun (setf thing-coordinates) (new-coordinates name)
  (setf (gethash name (positions *board*)) new-coordinates))

(defun set-thing-coordinates (name x y)
  (setf (thing-coordinates name) (list x y)))

(defun thing-cell (name)
  (multiple-value-bind (x y) (thing-coordinates name)
    (when (in-board x y)
      (cell x y))))

(defun (setf thing-cell) (new-cell name)
  (multiple-value-bind (x y) (thing-coordinates name)
    (when (in-board x y)
      (setf (cell x y) new-cell))))

(defun tile-rem-thing (name)
  (tile-rem$ (thing-cell name) name)
  (setf (thing-coordinates name) NIL))

(defun tile-add-thing (x y name &optional (state T))
  (when (in-board x y)
    (tile-rem-thing name)
    (tile-add x y name state)
    (set-thing-coordinates name x y)))

(defun move (name dx dy &optional wrap-x wrap-y check-hook)
  (multiple-value-bind (x y) (thing-coordinates name)
    (when (and (in-board x y))
      (let ((x* (if wrap-x
                    (mod (+ x dx) (board-width))
                    (+ x dx)))
            (y* (if wrap-y
                    (mod (+ y dx) (board-height))
                    (+ y dy))))
        (when (and (in-board x* y*)
                   (or (not check-hook)
                       (funcall check-hook (list x y) (list x* y*))))
          (tile-add-thing x* y* name))))))
