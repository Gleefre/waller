(in-package #:waller)

(defclass menu ()
  ((levels :initform (get-levels) :initarg :levels :accessor levels)
   (page   :initform 1 :initarg :page :accessor page)
   (width  :initform 3 :initarg :width :accessor menu-width)
   (height :initform 2 :initarg :height :accessor menu-height)
   (game   :initform NIL :initarg :game :accessor menu-game)
   (hovers :initform (make-hash-table) :accessor menu-hovers)))

(defun level (menu n)
  (load-board (aref (levels menu) n)))

(defun hover (menu n)
  (gethash n (menu-hovers menu)))

(defun (setf hover) (state menu n)
  (setf (gethash n (menu-hovers menu)) state))

(defun levels-per-page (menu)
  (* (menu-width menu) (menu-height menu)))

(defun make-menu ()
  (make-instance 'menu))

(defun draw-menu (menu width height)
  (s:background (c :menu-background))
  (let ((unit (min (/ width (menu-width menu)) (/ height (menu-height menu)))))
    (sf:with-fit ((* unit (menu-width menu)) (* unit (menu-height menu)) width height)
      (loop for n from (* (1- (page menu)) (levels-per-page menu)) below (length (levels menu))
            for pos below (levels-per-page menu)
            for pos-x = (mod pos (menu-width menu))
            for pos-y = (floor pos (menu-width menu))
            do (with-translate ((* unit pos-x) (* unit pos-y))
                 (with-color ((if (hover menu pos)
                                  (c :menu-button-hover)
                                  (c :menu-button)))
                   (let ((x 0) (y 0) (w unit) (h unit))
                     (with-margin (x w (c :menu-margin))
                       (with-margin (y h (c :menu-margin))
                         (s:rect x y w h)
                         (binds (brect x y w h)
                           :press (let ((n n))
                                    (lambda (b)
                                      (declare (ignore b))
                                      (setf (menu-hovers menu) (make-hash-table))
                                      (setf (game-board  (menu-game menu)) (level menu n)
                                            (game-screen (menu-game menu)) :board)))
                           :hover (let ((pos pos))
                                    (lambda ()
                                      (setf (hover menu pos) T)))
                           :unhover (let ((pos pos))
                                      (lambda ()
                                        (setf (hover menu pos) NIL))))))
                     (s:with-font (s:make-font :color (c :menu-font-color) :size (/ unit 3) :align :center)
                       (s:text (princ-to-string (1+ n))
                               (/ w 2) (/ unit 3))))))))))
