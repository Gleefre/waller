(in-package #:waller)

(defc
  :default-font "RobotoMono-Bold.ttf"
  :error-font   "RobotoMono-Bold.ttf")

(defc
  :level-file-template "levels/~a.brd"
  :level-cleared-templay "progress/~a.cleared"
  :soundtrack    "soundtrack.ogg"
  :note-template "notes/note-~a.ogg"
  :notes-range   '(-21 27)
  :sfx-volume 0.1)

(defc
  :select s:+magenta+
  :cloud (s:gray 0.7)
  :cloud-image "cloud.png")

(defc
  :hero-animation-clock (sc:make-clock)
  :food-animation-clock (sc:make-clock :speed 1.25)
  :win-animation-time 1/4
  :win-font-color (s:rgb 0.3 0.1 0.8)
  :win-font-size 1/15)

(defc
  :tile        (s:rgb 0.8 1.0 0.8)
  :margin      1/12
  :food-size   1/6
  :food-drad   1/3
  :food-weight 1/20)

(defc :A (s:rgb 0.8 0.4 0.2)
      :B (s:rgb 0.3 0.8 0.2))

(defc
  :background               (s:rgb 0.25 0.8 0.8)
  :menu-background          (s:rgb 0.25 0.9 0.9)
  :win-animation-background (s:rgb 0.3 0.8 0.2 0.9))

(defc
  :menu-margin       1/10
  :menu-font-color   (s:rgb 0.3 0.1 0.8)
  :cleared           (s:rgb 0.3 1.0 0.2)
  :buttons           (s:rgb 0.2 0.7 0.3)
  :buttons-disabled  (s:rgb 0.4 0.6 0.1)
  :buttons-hover     (s:rgb 0.4 0.9 0.5)
  :arrow             (s:rgb 0.3 0.4 0.7)
  :arrow-disabled    (s:rgb 0.7 0.3 0.2)
  :arrow-hover       (s:rgb 0.4 0.5 0.9)
  :menu-button       (c :buttons)
  :menu-button-hover (c :buttons-hover))
