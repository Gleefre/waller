(in-package #:cl-user)

(ql:quickload '(:deploy :sketch :harmony))

(push :deploy *features*)

(asdf:load-asd (merge-pathnames "waller.asd" (uiop:getcwd)))

(ql:quickload :waller)

(deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)
(deploy:define-resource-directory data "res/")

(asdf:make :waller)
