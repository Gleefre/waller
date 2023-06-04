(in-package #:waller)

(defun move-hero (direction)
  (case direction
    (:up (unless (tilep$ (thing-cell :hero) :uwall)
           (move :hero 0 -1 :check-hook #'check-tile)))
    (:down (unless (tilep$ (thing-cell :hero) :dwall)
             (move :hero 0 1 :check-hook #'check-tile)))
    (:right (unless (tilep$ (thing-cell :hero) :rwall)
              (move :hero 1 0 :check-hook #'check-tile)))
    (:left (unless (tilep$ (thing-cell :hero) :lwall)
             (move :hero -1 0 :check-hook #'check-tile)))))
