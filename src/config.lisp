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
