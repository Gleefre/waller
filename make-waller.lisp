(in-package #:cl-user)
(load (merge-pathnames "deploy.lisp" (uiop:getcwd)))
(asdf:make :waller)
