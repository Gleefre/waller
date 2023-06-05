(in-package #:waller)

(defvar *game*)

(s:defsketch game ((menu (make-menu))
                   (level)
                   (screen :menu))
  (let ((*game* s::*sketch*))
    (case screen
      (:menu  (draw-menu menu s:width s:height))
      (:level (with-board ((level-board level))
                (s:background (c :background))
                (draw-board *board* s:width s:height))))))

(defun game-board (game)
  (when (eq (game-screen game) :level)
    (level-board (game-level game))))

(defmethod kit.sdl2:mousebutton-event :around ((game game) state timestamp button x y)
  (let ((*game* game))
    (with-board ((game-board game))
      (call-next-method))))

(defmethod kit.sdl2:keyboard-event :around ((game game) state timestamp repeat-p keysym)
  (let ((*game* game))
    (with-board ((game-board game))
      (call-next-method))))

(defmethod kit.sdl2:keyboard-event ((game game) state timestamp repeat-p keysym)
  (when (eq state :keydown)
    (case (game-screen game)
      (:level
       (case (sdl2:scancode keysym)
         ((:scancode-up :scancode-w) (move-hero :up))
         ((:scancode-down :scancode-s) (move-hero :down))
         ((:scancode-right :scancode-d) (move-hero :right))
         ((:scancode-left :scancode-a) (move-hero :left))
         ((:scancode-q) (setf (game-screen game) :menu
                              (game-level game) NIL))
         ((:scancode-r) (level-reset (game-level game)))))
      (:menu
       (case (sdl2:scancode keysym)
         ((:scancode-left :scancode-n) (menu-next (game-menu game)))
         ((:scancode-right :scancode-p) (menu-previous (game-menu game)))
         ((:scancode-q) (kit.sdl2:close-window game)))))))

(s:define-start-function (start) game (:resizable T :title "Waller" :width 800 :height 800)
  (:start
    (music-init)
    (play-soundtrack))
  (:on-close (game)
    (mute-soundtrack)))
