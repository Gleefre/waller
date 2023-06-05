(defpackage #:waller
  (:use #:cl)
  (:export #:start
           #:start-toplevel
           #:editor-toplevel)
  (:local-nicknames (#:s  #:sketch)
                    (#:sf #:sketch-fit)
                    (#:sc #:stopclock)
                    (#:h  #:org.shirakumo.fraf.harmony)
                    (#:m  #:org.shirakumo.fraf.mixed)))
