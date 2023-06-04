(in-package #:waller)

(defparameter *features-to-edit* '(:tile :uwall :dwall :rwall :lwall :cloud))
(defparameter *things-to-move*   '(:hero))

(defun edit-cell (x y mode &optional (button 1))
  (cond ((member mode *features-to-edit*)
         (tile-toggle x y mode))
        ((member mode *things-to-move*)
         (if (= button 1)
             (tile-add-thing x y mode)
             (tile-rem-thing mode)))))

(defc :select s:+magenta+)

(define-cell-drawer :select (state unit)
  (when state
    (s:with-pen (s:make-pen :stroke (c :select) :weight 5)
      (s:rect 0 0 unit unit))))

(s:defsketch editor ((board *board*) (mode  :none))
  (with-board (board)
    (s:background (c :background))
    (let* ((unit (min (/ s:width (+ 2 (board-width)))
                      (/ s:height (max (board-height)
                                       (* 3/2
                                          (+ (length *features-to-edit*)
                                             (length *things-to-move*)))))))
           (bw (* unit (board-width)))
           (bh (* unit (board-height))))
      ;; board
      (with-translate ((* 2 unit) 0)
        (sf:with-fit (bw bh (- s:width (* 2 unit)) s:height)
          (draw-board board bw bh)
          (do-cells (:x x :y y)
            (with-translate ((* unit x) (* unit y))
              (bind (brect 0 0 unit unit) :release (alexandria:curry 'edit-cell x y mode))))))
      ;; tools
      (sf:with-fit (unit (* unit 3/2 (+ (length *features-to-edit*)
                                        (length *things-to-move*)))
                    (* 2 unit) s:height)
        (loop for feature in (append *features-to-edit* *things-to-move*)
              do (s:translate 0 (/ unit 4))
                 (draw-cell (list feature T :tile T :select (eq mode feature)) unit)
                 (bind (brect 0 0 unit unit) :release (let ((feature feature))
                                                        (lambda (b)
                                                          (declare (ignore b))
                                                          (setf mode feature))))
                 (s:translate 0 unit)
                 (s:translate 0 (/ unit 4)))))))

(defmethod kit.sdl2:mousebutton-event :around ((editor editor) state timestamp button x y)
  (with-board ((editor-board editor))
    (call-next-method)))
