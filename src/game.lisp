(in-package #:waller)

(defvar *game*)

(defun draw-home-button (unit &optional no-buttons)
  (let ((x 0) (y 0) (w unit) (h unit))
    (with-margin (x w (c :menu-margin))
      (with-margin (y h (c :menu-margin))
        (with-color ((c (if (hover (game-menu *game*) :home)
                            :buttons-hover
                            :buttons)))
          (s:rect x y w h)
          (with-translate (x y)
            (with-color ((c (if (hover (game-menu *game*) :home)
                                :arrow-hover
                                :arrow)))
              (s:polygon (/ w 2) (/ h 8)
                         (/ w 8) (/ h 3)
                         (/ w 8) (/ h 8/7)
                         (/ w 8/7) (/ h 8/7)
                         (/ w 8/7) (/ h 3))))
          (unless no-buttons
            (binds (brect x y w h)
              :press (lambda (b)
                       (declare (ignorable b))
                       (setf (game-screen *game*) :menu
                             (game-level *game*) NIL))
              :hover (lambda () (setf (hover (game-menu *game*) :home) T))
              :unhover (lambda () (setf (hover (game-menu *game*) :home) NIL)))))))))

(defun draw-reset-button (unit &optional no-buttons)
  (let ((x 0) (y 0) (w unit) (h unit))
    (with-margin (x w (c :menu-margin))
      (with-margin (y h (c :menu-margin))
        (with-translate (x y)
          (with-color ((c (if (hover (game-menu *game*) :reset)
                              :buttons-hover
                              :buttons)))
            (s:rect 0 0 w h))
          (with-color ((c (if (hover (game-menu *game*) :reset)
                              :arrow-hover
                              :arrow)))
            (s:circle (/ w 2) (/ h 2) (/ unit 4)))
          (with-color ((c (if (hover (game-menu *game*) :reset)
                              :buttons-hover
                              :buttons)))
            (s:circle (/ w 2) (/ h 2) (/ unit 5))
            (s:rect 0 0 (/ w 2) (/ h 2)))
          (with-color ((c (if (hover (game-menu *game*) :reset)
                              :arrow-hover
                              :arrow)))
            (s:polygon (/ w 2) (/ unit 9)
                       (/ w 2) (- (/ unit 2)
                                  (/ unit 4))
                       (/ w 3) (/ unit 5.5))))
        (unless no-buttons
          (binds (brect x y w h)
            :press (lambda (b)
                     (declare (ignorable b))
                     (level-reset (game-level *game*)))
            :hover (lambda () (setf (hover (game-menu *game*) :reset) T))
            :unhover (lambda () (setf (hover (game-menu *game*) :reset) NIL))))))))

(defun draw-level (level width height &optional no-buttons)
  (with-board ((level-board level))
    (s:background (c :background))
    (let ((unit (min (/ width (max 4 (board-width)))
                     (/ height (+ 1 (board-height))))))
      (draw-board *board* width (- height unit))
      (with-translate (0 (- height unit))
        (sf:with-fit ((* unit 4) unit width unit)
          (s:translate (/ unit 2) 0)
          (draw-home-button unit no-buttons)
          (s:translate (* unit 2) 0)
          (draw-reset-button unit no-buttons))))))

(defun draw-win (width height
                 &aux (x (min 1 (sc:time (c :win-animation))))
                      (x (expt x 1/3)))
  (with-scale (x x (/ width 2) (/ height 2))
    (with-color ((c :win-animation-background))
      (s:rect 0 0 width height)
      (binds (brect 0 0 width height)
        :press (lambda (b) (declare (ignore b)) (exit-win))))
    (s:with-font (s:make-font :color (c :win-font-color) :size (* width (c :win-font-size)) :align :center)
      (s:text "Level cleared!

[press any key to continue]" (/ width 2) (/ height 3)))))

(defun exit-win ()
  (when (> (sc:time (c :win-animation)) 1)
    (setf (game-screen *game*) :menu
          (game-level *game*) NIL)))

(s:defsketch game ((menu (make-menu))
                   (level)
                   (screen :menu))
  (let ((*game* s::*sketch*))
    (case screen
      (:menu  (draw-menu menu s:width s:height))
      (:level (draw-level level s:width s:height))
      (:win
       (draw-level level s:width s:height T)
       (draw-win s:width s:height)))))

(defun game-board (game)
  (when (eq (game-screen game) :level)
    (level-board (game-level game))))

(defmethod kit.sdl2:mousebutton-event :around ((game game) state timestamp button x y)
  (let ((*game* game))
    (with-board ((game-board game))
      (call-next-method))))

(defmethod kit.sdl2:mousemotion-event :around ((game game) state button-mask x y xrel yrel)
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
         ((:scancode-r) (level-reset (game-level game)))
         ((:scancode-m) (toggle-soundtrack))
         ((:scancode-f) (setf *sfx-mute* (not *sfx-mute*))))
       (when (check-win)
         (setf (game-screen game) :win
               (c :win-animation) (sc:make-clock :speed (/ (c :win-animation-time))))
         (level-clear (game-level game))))
      (:win (exit-win))
      (:menu
       (case (sdl2:scancode keysym)
         ((:scancode-right :scancode-n) (menu-next (game-menu game)))
         ((:scancode-left :scancode-p) (menu-previous (game-menu game)))
         ((:scancode-q) (kit.sdl2:close-window game))
         ((:scancode-m) (toggle-soundtrack))
         ((:scancode-f) (setf *sfx-mute* (not *sfx-mute*))))))))

(s+:define-start-function (start) game (:resizable T :title "Waller" :width 800 :height 800)
  (:start
    (music-init)
    (play-soundtrack))
  (:on-close (game)
    (mute-soundtrack)))
