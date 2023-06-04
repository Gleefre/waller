(in-package #:waller)

(defparameter *cell-drawers* (make-hash-table))
(defparameter *cell-draw-order* '(:tile :hero :uwall :dwall :rwall :lwall :cloud :select))

(defmacro define-cell-drawer (feature-name (state unit) &body body)
  `(setf (gethash ,feature-name *cell-drawers*)
         (lambda (,state ,unit)
           ,@body)))

(defun call-cell-drawer (cell feature-name unit)
  (funcall (gethash feature-name *cell-drawers*)
           (tilep$ cell feature-name)
           unit))

(defun draw-cell (cell unit)
  (dolist (feature-name *cell-draw-order*)
    (s:with-current-matrix
      (call-cell-drawer cell feature-name unit))))

(define-cell-drawer :tile (state unit)
  (when state
    (with-color ((c :tile))
      (s:rect (* unit           (c :margin))   (* unit           (c :margin))
              (* unit (- 1 (* 2 (c :margin)))) (* unit (- 1 (* 2 (c :margin))))))))

(defc :hero-animation-clock (sc:make-clock))

(define-cell-drawer :hero (state unit)
  (when state
    (with-color ((c :hero))
      (s:ellipse (/ unit 2) (/ unit 2)
                 (* unit (* 1/3 (sin (sc:time (c :hero-animation-clock)))))
                 (* unit 1/3)))))

(define-cell-drawer :uwall (state unit)
  (when state
    (with-color ((c :wall))
      (s:rect 0 0 unit (* unit (c :margin))))))

(define-cell-drawer :dwall (state unit)
  (when state
    (with-color ((c :wall))
      (s:rect 0 (* unit (- 1 (c :margin))) unit (* unit (c :margin))))))

(define-cell-drawer :rwall (state unit)
  (when state
    (with-color ((c :wall))
      (s:rect (* unit (- 1 (c :margin))) 0 (* unit (c :margin)) unit))))

(define-cell-drawer :lwall (state unit)
  (when state
    (with-color ((c :wall))
      (s:rect 0 0 (* unit (c :margin)) unit))))

(define-cell-drawer :cloud (state unit)
  (when state
    (with-color ((c :cloud))
      (s:rect 0 0 unit unit)
      (s:image (s:load-resource (data-path (c :cloud-image)))
               0 0 unit unit))))

(defun draw-board (board width height)
  (with-board (board)
    (let ((unit (min (/ width (board-width)) (/ height (board-height)))))
      (sf:with-fit ((* unit (board-width)) (* unit (board-height)) width height)
        (do-cells (:cell cell :x x :y y)
          (with-translate ((* unit x) (* unit y))
            (draw-cell cell unit)))))))
