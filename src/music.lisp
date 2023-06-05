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
  (setf (note x) (h:create (note-file x) :mixer :effect :volume (c :sfx-volume))))

(defun create-notes ()
  (destructuring-bind (min max) (c :notes-range)
    (loop for x from min to max
          do (create-note x))))

(defun create-soundtrack ()
  (setf *soundtrack*
        (make-instance 'h:environment :sets `((:normal ,(probe-file (data-path (c :soundtrack))))))))

(defun play-soundtrack ()
  (h:transition *soundtrack* :normal))

(defun mute-soundtrack ()
  (h:transition *soundtrack* NIL))

(defparameter *soundtrack-mute* NIL)

(defun toggle-soundtrack ()
  (if *soundtrack-mute*
      (progn
        (play-soundtrack)
        (setf *soundtrack-mute* NIL))
      (progn
        (mute-soundtrack)
        (setf *soundtrack-mute* T))))

(defparameter *sfx-mute* NIL)

(defun sfx (&rest notes)
  (unless *sfx-mute*
    (dolist (x notes)
      (h:play (note x) :reset T))))

(defun music-init ()
  (unless h:*server*
    (h:maybe-start-simple-server :mixers '((:music m:basic-mixer) (:effect m:basic-mixer))
                                 :name "Waller")
    (create-notes)
    (create-soundtrack)))
