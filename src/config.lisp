(in-package #:waller)

(defc :default-font "RobotoMono-Bold.ttf")
(defc :error-font "RobotoMono-Bold.ttf")

(defc
  :background  s:+black+
  :tile        s:+white+
  :cloud       (s:gray 0.7)
  :cloud-image "cloud.png"
  :wall        (s:gray 0.3)
  :margin 1/10)
