(require :uiop)
(require :asdf)

(ql:quickload :deploy)
(load "waller.asd")
(push :deploy *features*)
(ql:quickload :waller)

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
            org.shirakumo.fraf.mixed.coreaudio.cffi::audio-unit)
 ;; Can't ship SDL2 - too many dependencies
 #+linux (sdl2::libsdl2
          sdl2-image::libsdl2-image
          sdl2-ttf::libsdl2-ttf))
