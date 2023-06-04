(in-package #:waller)

;; c and defc for easy configuration
(let ((colors (make-hash-table)))
  (defun c (name)
    (gethash name colors))
  (defun (setf c) (color name)
    (setf (gethash name colors) color)))

(defmacro defc (&body name-value-clauses &key &allow-other-keys)
  `(setf ,@(loop for (name value) on name-value-clauses by #'cddr
                 collect `(c ,name)
                 collect value)))

;; data-path to get resource's path
(defparameter *data-location* "res/")

(let ((data-folder nil))
  (defun data-path (relative-path)
    (setf data-folder
          (or data-folder
              #-deploy (asdf:system-relative-pathname "waller" *data-location*)
              #+deploy (let ((deploy:*data-location* *data-location*))
                         (deploy:data-directory))))
    (format nil "~a" (merge-pathnames relative-path data-folder))))
