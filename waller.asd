(asdf:defsystem "waller"
  :description "Waller - Game for Lisp Game Jam 2023"
  :version "0.2.0"
  :author "Gleefre <varedif.a.s@gmail.com>"
  :licence "Apache 2.0"

  :depends-on ("sketch" "sketch-fit" "stopclock" "harmony" "stealth-mixin" "alexandria")

  :pathname "src"
  :components ((:file "packages")
               (:file "sketch-buttons"))

  :defsystem-depends-on (:deploy)
  :build-operation #-darwin "deploy-op" #+darwin "osx-app-deploy-op"
  :build-pathname "waller"
  :entry-point "waller:start-toplevel")
