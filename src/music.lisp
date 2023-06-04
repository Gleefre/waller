(in-package #:waller)

(defun note-file (x)
  (probe-file (data-path (format nil (c :note-template) x))))

(defparameter *notes* (make-hash-table))
(defparameter *soundtrack* NIL)

(defun note (x)
  (gethash x *notes*))

(defun (setf note) (note x)
  (setf (gethash x *notes*) note))

(defun create-note (x)
  (setf (note x) (h:create (note-file x) :mixer :effect)))

(defun create-notes ()
  (destructuring-bind (min max) (c :notes-range)
    (loop for x from min to max
          do (create-note x))))

(defun create-soundtrack ()
  (setf *soundtrack* (h:create (pathname (data-path (c :soundtrack))) :mixer :music)))

(defun play-soundtrack ()
  (h:play *soundtrack* :reset T))

(defun mute-soundtrack ()
  (h:stop *soundtrack*))

(defun sfx (&optional (x 3))
  (h:play (note x) :reset T))

(defun music-init ()
  (h:maybe-start-simple-server :mixers '((:music m:basic-mixer) (:effect m:basic-mixer)))
  (create-notes)
  (create-soundtrack))
