(in-package #:waller)

(defc :default-font "RobotoMono-Bold.ttf"
      :error-font   "RobotoMono-Bold.ttf")

(defc :background  s:+black+
      :tile        s:+white+
      :hero        s:+green+
      :wall        s:+green+
      :cloud       (s:gray 0.7)
      :cloud-image "cloud.png"
      :margin      1/15)

(defc :menu-background   (s:gray 0.2)
      :menu-button       (s:rgb 0.3 0.7 0.3)
      :menu-button-hover (s:rgb 0.5 0.9 0.5)
      :menu-margin       1/5
      :menu-font-color   s:+black+)

(defc :soundtrack    "soundtrack.wav"
      :note-template "notes/note-~a.wav"
      :notes-range   '(-21 27))
