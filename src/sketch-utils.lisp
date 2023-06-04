(in-package #:waller)

;; with-color macro (creates pen for you)
(defmacro with-color ((color &optional (type :fill)) &body body)
  `(s:with-pen (s:make-pen ,type ,color)
     ,@body))

;; Basic with- macros for translate, rotate and scale sketch functions
(defmacro with-translate ((dx dy) &body body)
  `(s:with-current-matrix
     (s:translate ,dx ,dy)
     ,@body))

(defmacro with-rotate ((angle &optional (cx 0) (cy 0)) &body body)
  `(s:with-current-matrix
     (rotate ,angle ,cx ,cy)
     ,@body))

(defmacro with-scale ((sx &optional sy (cx 0) (cy 0)) &body body)
  `(s:with-current-matrix
     (scale ,sx ,sy ,cx ,cy)
     ,@body))

;; Use custom font as the default one
(let ((font))
  (defun s::make-default-font ()
    (setf font (or font
                   (s:make-font :face (s:load-resource (data-path (c :default-font)))
                                :color s:+black+
                                :size 18)))))

(let ((font))
  (defun s::make-error-font ()
    (setf font (or font
                   (s:make-font :face (s:load-resource (data-path (c :error-font)))
                                :color s:+red+
                                :size 16)))))
