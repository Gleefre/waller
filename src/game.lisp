(in-package #:waller)

(s:defsketch game ((board *board*))
  (with-board (board)
    (s:background (c :background))
    (draw-board board s:width s:height)))

(defmethod kit.sdl2:mousebutton-event :around ((game game) state timestamp button x y)
  (with-board ((game-board game))
    (call-next-method)))

(defmethod kit.sdl2:keyboard-event :around ((game game) state timestamp repeat-p keysym)
  (with-board ((game-board game))
    (call-next-method)))

(defun check-tile (from to)
  (declare (ignorable from))
  (apply #'tilep to))

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

(defmethod kit.sdl2:keyboard-event ((game game) state timestamp repeat-p keysym)
  (when (eq state :keydown)
    (case (sdl2:scancode keysym)
      ((:scancode-up :scancode-w) (move-hero :up))
      ((:scancode-down :scancode-s) (move-hero :down))
      ((:scancode-right :scancode-d) (move-hero :right))
      ((:scancode-left :scancode-a) (move-hero :left)))))
