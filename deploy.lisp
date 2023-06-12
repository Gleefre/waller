(in-package #:cl-user)

(push :deploy *features*)
(asdf:load-asd (merge-pathnames "waller.asd" (uiop:getcwd)))
(ql:quickload '(:deploy :waller))

(deploy:define-resource-directory data "res/")

(defmacro dont-deploy (&rest libraries)
  `(progn
     ,@(loop for library in (alexandria:flatten libraries)
             collect `(deploy:define-library ,library :dont-deploy T))))

(defmacro deploy (&rest names)
  `(progn
     ,@(loop for name in (alexandria:flatten names)
             for library = (gensym)
             collect `(cffi:define-foreign-library ,library (:linux ,name))
             collect `(deploy:define-library ,library :dont-deploy NIL))))

(dont-deploy
 cl-opengl-bindings::opengl
 #+linux   (org.shirakumo.fraf.mixed.pulse.cffi::libpulse-simple
            org.shirakumo.fraf.mixed.pulse.cffi::libpulse
            org.shirakumo.fraf.mixed.alsa.cffi::libasound)
 #+windows (org.shirakumo.fraf.mixed.winmm.cffi::winmm
            org.shirakumo.fraf.mixed.wasapi.cffi::avrt)
 #+darwin  (org.shirakumo.fraf.mixed.coreaudio.cffi::audio-toolbox
            org.shirakumo.fraf.mixed.coreaudio.cffi::audio-unit))
