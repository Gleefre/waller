(in-package #:waller)

(s:defsketch game ((board)
                   (menu (make-menu))
                   (screen :menu))
  (case screen
    (:menu  (draw-menu menu s:width s:height))
    (:board (with-board (board)
              (s:background (c :background))
              (draw-board board s:width s:height)))))

(defmethod kit.sdl2:mousebutton-event :around ((game game) state timestamp button x y)
  (with-board ((game-board game))
    (call-next-method)))

(defmethod kit.sdl2:keyboard-event :around ((game game) state timestamp repeat-p keysym)
  (with-board ((game-board game))
    (call-next-method)))

(defun check-tile (from to)
  (declare (ignorable from))
  (apply #'tilep to))

(defmethod kit.sdl2:keyboard-event ((game game) state timestamp repeat-p keysym)
  (when (eq state :keydown)
    (case (game-screen game)
      (:board
       (case (sdl2:scancode keysym)
         ((:scancode-up :scancode-w) (move-hero :up))
         ((:scancode-down :scancode-s) (move-hero :down))
         ((:scancode-right :scancode-d) (move-hero :right))
         ((:scancode-left :scancode-a) (move-hero :left))
         ((:scancode-q) (setf (game-screen game) :menu
                              (game-board game) NIL))))
      (:menu
       (case (sdl2:scancode keysym)
         ((:scancode-left :scancode-n) (incf (page (game-menu game))))
         ((:scancode-right :scancode-p) (decf (page (game-menu game))))
         ((:scancode-q) (kit.sdl2:close-window game)))))))

(s:define-start-function (start) game (:resizable T :title "Waller")
  (:start
    (music-init)
    (play-soundtrack))
  (:on-close (game)
    (mute-soundtrack))
  (:setup (game)
    (setf (menu-game (game-menu game)) game)))
