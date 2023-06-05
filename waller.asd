(asdf:defsystem "waller"
  :description "Waller - Game for Lisp Game Jam 2023"
  :version "0.3.0"
  :author "Gleefre <varedif.a.s@gmail.com>"
  :licence "Apache 2.0"

  :depends-on ("sketch"
               "sketch-fit" "stopclock"
               "stealth-mixin" "alexandria"
               "harmony" "cl-mixed-vorbis"
               #+(and linux (not android)) "cl-mixed-pulse"
               #+android "cl-mixed-aaudio"
               #+darwin "cl-mixed-coreaudio"
               #+windows "cl-mixed-wasapi"
               #+bsd "cl-mixed-oss")

  :pathname "src"
  :serial T
  :components ((:file "packages")
               (:file "utils")
               (:file "config")
               (:file "sketch-utils")
               (:file "sketch-buttons")
               (:file "board")
               (:file "save-board")
               (:file "draw-board")
               (:file "editor")
               (:file "levels")
               (:file "menu")
               (:file "music")
               (:file "main")
               (:file "game"))

  :defsystem-depends-on (:deploy)
  :build-operation #-darwin "deploy-op" #+darwin "osx-app-deploy-op"
  :build-pathname "waller"
  :entry-point "waller:start-toplevel")
