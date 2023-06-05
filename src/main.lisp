(in-package #:waller)

(defun check-tile (from to)
  (declare (ignorable from))
  (apply #'tilep to))

(defun move-hero (direction)
  (unless (eq (tilep$ (thing-cell :hero)
                      (case direction
                        (:up :uwall)
                        (:down :dwall)
                        (:right :rwall)
                        (:left :lwall)))
              (thing-state :hero))
    (case direction
      (:up (move :hero 0 -1 :check-hook #'check-tile))
      (:down (move :hero 0 1 :check-hook #'check-tile))
      (:right (move :hero 1 0 :check-hook #'check-tile))
      (:left (move :hero -1 0 :check-hook #'check-tile)))))
