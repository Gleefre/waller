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

(defc :menu-background   (s:rgb 0.25 0.1 0.1)
      :menu-button       (s:rgb 0.3 0.7 0.3)
      :menu-button-hover (s:rgb 0.5 0.9 0.5)
      :menu-margin       1/10
      :menu-font-color   s:+black+
      :cleared           (s:rgb 0.2 0.3 0.9)
      :buttons           (s:rgb 0.1 0.5 0.2)
      :buttons-hover     (s:rgb 0.1 0.6 0.2)
      :buttons-disabled  (s:rgb 0.1 0.3 0.2)
      :arrow             (s:rgb 0.2 0.8 0.65)
      :arrow-hover       (s:rgb 0.2 0.8 0.7)
      :arrow-disabled    (s:rgb 0.1 0.2 0.3))

(defc :soundtrack    "soundtrack.ogg"
      :note-template "notes/note-~a.ogg"
      :notes-range   '(-21 27))

(defc :A (s:rgb 0.5 0.2 0.3)
      :B (s:rgb 0.2 0.5 0.3))

(defc :hero-animation-clock (sc:make-clock)
      :food-animation-clock (sc:make-clock :speed 1.25))

(defc :food-size 1/5
      :food-drad 1/3
      :food-weight 1/15)

(defc :sfx-volume 0.1)

(defc :select s:+magenta+)
