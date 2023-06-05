(in-package #:waller)

(defclass menu ()
  ((levels :initform (get-levels) :initarg :levels :accessor levels)
   (page   :initform 1 :initarg :page :accessor page)
   (width  :initform 3 :initarg :width :accessor menu-width)
   (height :initform 2 :initarg :height :accessor menu-height)
   (hovers :initform (make-hash-table) :accessor menu-hovers)))

(defun level (menu n)
  (aref (levels menu) n))

(defun hover (menu n)
  (gethash n (menu-hovers menu)))

(defun (setf hover) (state menu n)
  (setf (gethash n (menu-hovers menu)) state))

(defun per-page (menu)
  (* (menu-width menu) (menu-height menu)))

(defun make-menu ()
  (make-instance 'menu))

(defun enter-level (level)
  (level-reset level)
  (setf (game-level *game*) level
        (game-screen *game*) :level))

(defun menu-next-p (menu)
  (not (>= (* (page menu) (per-page menu))
           (length (levels menu)))))

(defun menu-next (menu)
  (when (menu-next-p menu)
    (incf (page menu))))

(defun menu-previous-p (menu)
  (not (<= (page menu) 1)))

(defun menu-previous (menu)
  (when (menu-previous-p menu)
    (decf (page menu))))

(defun draw-menu (menu width height)
  (s:background (c :menu-background))
  (let ((unit (min (/ width (+ 2/3 (menu-width menu)))
                   (/ height (+ 1 (menu-height menu))))))
    (sf:with-fit ((* unit (+ 2/3 (menu-width menu))) (* unit (+ 1 (menu-height menu)))
                  width height)
      (with-translate ((/ unit 3) 0)
        (with-translate ((- (/ unit 3)) (* (+ 1 (menu-height menu) -5/6) unit))
          (let ((x 0) (y 0) (w unit) (h (* 2/3 unit)))
            (with-color ((c (cond ((not (menu-previous-p menu)) :buttons-disabled)
                                  ((hover menu :previous) :buttons-hover)
                                  (T :buttons))))
              (with-margin (x w (c :menu-margin))
                (with-margin (y h (c :menu-margin))
                  (s:rect x y w h)
                  (with-translate (x y)
                    (with-color ((c (cond ((not (menu-previous-p menu)) :arrow-disabled)
                                          ((hover menu :previous) :arrow-hover)
                                          (T :arrow))))
                      (s:rect (/ w 3) (/ h 3) (/ w 2) (/ h 3))
                      (s:polygon (/ w 2) (/ h 6)
                                 (/ w 6) (/ h 2)
                                 (/ w 2) (* h 5/6))))
                  (binds (brect x y w h)
                    :press (lambda (b)
                             (declare (ignorable b))
                             (menu-previous menu))
                    :hover (lambda ()
                             (setf (hover menu :previous) T))
                    :unhover (lambda ()
                               (setf (hover menu :previous) NIL))))))))
        (with-translate ((* unit (- (menu-width menu) 2/3)) (* (+ 1 (menu-height menu) -5/6) unit))
          (let ((x 0) (y 0) (w unit) (h (* 2/3 unit)))
            (with-color ((c (cond ((not (menu-next-p menu)) :buttons-disabled)
                                  ((hover menu :next) :buttons-hover)
                                  (T :buttons))))
              (with-margin (x w (c :menu-margin))
                (with-margin (y h (c :menu-margin))
                  (s:rect x y w h)
                  (with-translate (x y)
                    (with-color ((c (cond ((not (menu-next-p menu)) :arrow-disabled)
                                          ((hover menu :next) :arrow-hover)
                                          (T :arrow))))
                      (with-scale (-1 1 (/ w 2) 0)
                        (s:rect (/ w 3) (/ h 3) (/ w 2) (/ h 3))
                        (s:polygon (/ w 2) (/ h 6)
                                   (/ w 6) (/ h 2)
                                   (/ w 2) (* h 5/6)))))
                  (binds (brect x y w h)
                    :press (lambda (b)
                             (declare (ignorable b))
                             (menu-next menu))
                    :hover (lambda ()
                             (setf (hover menu :next) T))
                    :unhover (lambda ()
                               (setf (hover menu :next) NIL))))))))
        (loop for n from (* (1- (page menu)) (per-page menu)) below (length (levels menu))
              for pos below (per-page menu)
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
                                        (enter-level (level menu n))))
                             :hover (let ((pos pos))
                                      (lambda ()
                                        (setf (hover menu pos) T)))
                             :unhover (let ((pos pos))
                                        (lambda ()
                                          (setf (hover menu pos) NIL))))))
                       (s:with-font (s:make-font :color (c :menu-font-color) :size (/ unit 3) :align :center)
                         (s:text (princ-to-string (1+ n))
                                 (/ w 2) (/ unit 3)))
                       (when (level-cleared (level menu n))
                         (with-color ((c :cleared))
                           (s:circle (/ w 3) (/ unit 3) (/ unit 10))))))))))))
