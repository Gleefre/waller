(push :deploy-console *features*)
(load "deploy.lisp")
(asdf:make :waller/editor)
