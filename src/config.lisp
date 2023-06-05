(in-package #:waller)

(defc :default-font "RobotoMono-Bold.ttf"
      :error-font   "RobotoMono-Bold.ttf")

(defc :background  s:+black+
      :tile        s:+white+
      :hero        s:+green+
      :wall        s:+green+
      :cloud       (s:gray 0.7)
      :cloud-image "cloud.png"
      :margin      1/8)

(defc :menu-background   (s:gray 0.2)
      :menu-button       (s:rgb 0.3 0.7 0.3)
      :menu-button-hover (s:rgb 0.5 0.9 0.5)
      :menu-margin       1/5
      :menu-font-color   s:+black+)

(defc :soundtrack    "soundtrack.ogg"
      :note-template "notes/note-~a.ogg"
      :notes-range   '(-21 27))

(defc :A (s:rgb 0.5 0.2 0.3)
      :B (s:rgb 0.2 0.5 0.3))

(defc :hero-animation-clock (sc:make-clock)
      :food-animation-clock (sc:make-clock :speed 1.25))

(defc :food-size 1/5
      :food-drad 1/3
      :food-weight 5)

(defc :sfx-volume 0.1)
