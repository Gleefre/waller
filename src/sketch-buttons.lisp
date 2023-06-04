(in-package #:waller)

(defclass button ()
  ((on-press        :initform NIL :initarg :on-press   :type (or function null) :accessor on-press)
   (on-release      :initform NIL :initarg :on-release :type (or function null) :accessor on-release)
   (%inverse-matrix :initform (sb-cga:inverse-matrix (s::env-model-matrix s::*env*))
                    :accessor %inverse-matrix)))

(defgeneric button-contains (button x y))
(defgeneric button-event (button state mouse-button x y &rest args &key timestamp))
(defgeneric button-press (button mouse-button &rest args &key timestamp))
(defgeneric button-release (button mouse-button &rest args &key timestamp))

(defmethod button-contains ((button button) x y) T)

(defmethod button-press ((button button) mouse-button &key &allow-other-keys)
  (alexandria:when-let ((function (on-press button)))
    (funcall function mouse-button)))

(defmethod button-release ((button button) mouse-button &key &allow-other-keys)
  (alexandria:when-let ((function (on-release button)))
    (funcall function mouse-button)))

(defmethod button-event ((button button) state mouse-button x y &rest args &key &allow-other-keys)
  (when (button-contains button x y)
    (case state
      (:mousebuttondown (apply #'button-press button mouse-button args))
      (:mousebuttonup (apply #'button-release button mouse-button args)))))

(defmethod button-event :around ((button button) state mouse-button x y &rest args &key &allow-other-keys)
  (destructuring-bind (x y)
      (s::transform-vertex (list x y) (%inverse-matrix button))
    (apply #'call-next-method button state mouse-button x y args)))

(defgeneric bind (button type function))

(defmethod bind ((button button) (type (eql :press)) function)
  (setf (on-press button) function)
  button)

(defmethod bind ((button button) (type (eql :release)) function)
  (setf (on-release button) function)
  button)

(defmacro binds (button &body clauses &key &allow-other-keys)
  (if clauses
      `(binds (bind ,button ,(car clauses) ,(cadr clauses))
         ,@(cddr clauses))
      button))

(defvar *buttons*)

(defmethod initialize-instance :after ((button button) &key &allow-other-keys)
  (push button *buttons*))

(defmacro with-buttons ((&optional var) &body body)
  `(let ((*buttons* ,var))
     ,@body
     *buttons*))

(stealth-mixin:define-stealth-mixin sketch-buttons () s:sketch
  ((%buttons :initform NIL :accessor %buttons)))

(defmethod kit.sdl2:render :around ((app sketch-buttons))
  (setf (%buttons app)
        (with-buttons ()
          (call-next-method))))

(defmethod kit.sdl2:mousebutton-event :before ((app sketch-buttons) state timestamp mouse-button x y)
  (with-buttons ((%buttons app))
    (dolist (button *buttons*)
      (button-event button state mouse-button x y :timestamp timestamp))))

(defclass rectangle-button (button) ())

(defmethod initialize-instance :around ((button rectangle-button) &key x y w h &allow-other-keys)
  (s:with-current-matrix
    (s:translate x y)
    (s:scale w h)
    (call-next-method)))

(defmethod button-contains ((button rectangle-button) x y)
  (and (<= 0 x 1)
       (<= 0 y 1)))

(defun brect (x y w h)
  (when (and (plusp w) (plusp h))
    (make-instance 'rectangle-button :x x :y y :w w :h h)))

(defclass ellipse-button (button) ())

(defmethod initialize-instance :around ((button ellipse-button) &key x y rx ry &allow-other-keys)
  (s:with-current-matrix
    (s:translate x y)
    (s:scale rx ry)
    (call-next-method)))

(defmethod button-contains ((button ellipse-button) x y)
  (<= (+ (expt x 2) (expt y 2)) 1))

(defun bellipse (x y rx ry)
  (when (and (not (zerop rx)) (not (zerop ry)))
    (make-instance 'ellipse-button :x x :y y :rx rx :ry ry)))

(defun bcircle (x y r)
  (bellipse x y (abs r) (abs r)))
