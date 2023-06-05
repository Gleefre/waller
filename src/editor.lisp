(in-package #:waller)

(defparameter *features-to-edit* '((:tile T)
                                   (:cloud T)
                                   (:uwall :A :B)
                                   (:dwall :A :B)
                                   (:rwall :A :B)
                                   (:lwall :A :B)
                                   (:food  :A :B)))

(defparameter *things-to-move*   '((:hero :A :B)))

(defun edit-cell (x y mode state &optional (button 1))
  (cond ((member mode *features-to-edit* :key #'car)
         (tile-toggle x y mode state))
        ((member mode *things-to-move* :key #'car)
         (if (= button 1)
             (tile-add-thing x y mode state)
             (tile-rem-thing mode)))))

(define-cell-drawer :select (state unit)
  (when state
    (s:with-pen (s:make-pen :stroke (c :select) :weight 5)
      (s:rect 0 0 unit unit))))

(defmacro tool-hook (%feature %states mode mode-state
                     &aux (feature (gensym "feature"))
                          (states (gensym "states")))
  `(let ((,feature ,%feature)
         (,states (or ,%states (list T))))
     (lambda (b)
       (declare (ignore b))
       (cond ((not (eq ,mode ,feature))
              (setf ,mode ,feature
                    ,mode-state (car ,states)))
             ((cdr (member ,mode-state ,states))
              (setf ,mode-state (cadr (member ,mode-state ,states))))
             (T (setf ,mode :none ,mode-state T))))))

(s:defsketch editor ((level)
                     (mode :none)
                     (mode-state T)
                     (close))
  (when close
    (kit.sdl2:close-window s::*sketch*))
  (with-board ((level-board level))
    (s:background (c :background))
    (let* ((unit (min (/ s:width (+ 2 1/2 (board-width)))
                      (/ s:height (max (board-height)
                                       (* 3/2
                                          (+ (length *features-to-edit*)
                                             (length *things-to-move*)))))))
           (bw (* unit (board-width)))
           (bh (* unit (board-height))))
      ;; board
      (with-translate ((* 2 unit) 0)
        (sf:with-fit (bw bh (- s:width (* (+ 2 1/2) unit)) s:height)
          (s:with-pen (s:make-pen :stroke (c :select) :weight 3)
            (s:rect 0 0 bw bh))
          (draw-board *board* bw bh)
          (do-cells (:x x :y y)
            (with-translate ((* unit x) (* unit y))
              (bind (brect 0 0 unit unit) :release (alexandria:curry 'edit-cell x y mode mode-state))))))
      ;; tools
      (sf:with-fit (unit (* unit 3/2 (+ (length *features-to-edit*)
                                        (length *things-to-move*)))
                    (* 2 unit) s:height)
        (loop for (feature . states) in (append *features-to-edit* *things-to-move*)
              do (s:translate 0 (/ unit 4))
                 (draw-cell (list :select (eq mode feature)
                                  feature (if (eq mode feature)
                                              mode-state
                                              (car states))
                                  :tile T)
                            unit)
                 (bind (brect 0 0 unit unit) :release (tool-hook feature states mode mode-state))
                 (s:translate 0 unit)
                 (s:translate 0 (/ unit 4)))))))

(defmethod kit.sdl2:mousebutton-event :around ((editor editor) state timestamp button x y)
  (with-board ((level-board (editor-level editor)))
    (call-next-method)))

(defmethod kit.sdl2:keyboard-event :around ((editor editor) state timestamp repeat-p keysym)
  (with-board ((level-board (editor-level editor)))
    (call-next-method)))

(defmethod kit.sdl2:keyboard-event ((editor editor) state timestamp repeat-p keysym)
  (when (eq state :keydown)
    (case (sdl2:scancode keysym)
      ((:scancode-up) (move-hero :up))
      ((:scancode-down) (move-hero :down))
      ((:scancode-right) (move-hero :right))
      ((:scancode-left) (move-hero :left))
      ((:scancode-l) (level-save-board (editor-level editor)))
      ((:scancode-r) (level-reset (editor-level editor)))
      ((:scancode-q) (kit.sdl2:close-window editor)))))

(defparameter *help-text*
  (format nil
  "Usage:
   ./run-editor.~a [options] <level-number>
     <level-number>           Number of level to edit (must be a positive integer).
     -W, --width <number>     Create new level with given width (must be a positive integer).
                                (-H must be also present)
     -H, --height <number>    Create new level with given height (must be a positive integer).
                                (-W must be also present)
     -h, --help               Display this message"
  #+windows "bat" #-windows "sh"))

(defun editor-cmd-args ()
  (let ((args (uiop:command-line-arguments)))
    (when (or (null args)
              (evenp (length args))
              (member "-h" args :test #'string=)
              (member "--help" args :test #'string-equal)
              (member "-help" args :test #'string-equal)
              (not (or (and (= (length args) 1)
                            (let ((num (parse-integer (car args) :junk-allowed T)))
                              (and num (plusp num)
                                   (return-from editor-cmd-args (list :level-number num)))))
                       (and (= (length args) 5)
                            (let ((num-1 (parse-integer (second args) :junk-allowed T))
                                  (num-2 (parse-integer (fourth args) :junk-allowed T))
                                  (num (parse-integer (fifth args) :junk-allowed T)))
                              (and num (plusp num)
                                   num-1 (plusp num-1)
                                   num-2 (plusp num-2)
                                   (loop with w with h
                                         for arg in (list (first args) (third args))
                                         for arg-num in (list num-1 num-2)
                                         do (cond
                                              ((member arg '("-W" "-width" "--width") :test #'string-equal)
                                               (setf w arg-num))
                                              ((member arg '("-H" "-height" "--height") :test #'string-equal)
                                               (setf h arg-num))
                                              (T (return NIL)))
                                         finally (if (and w h)
                                                     (return-from editor-cmd-args (list :level-number num
                                                                                        :new T
                                                                                        :width w
                                                                                        :height h))
                                                     (return NIL)))))))))
      (return-from editor-cmd-args (list :help T)))))

(defmethod initialize-instance :after ((editor editor) &key &allow-other-keys)
  (when s::*build*
    (let ((args (editor-cmd-args)))
      (cond
        ((getf args :help)
         (format t "~a~%" *help-text*)
         (setf (editor-close editor) T))
        (T
         (setf (editor-level editor)
               (make-level (getf args :level-number)
                           (when (getf args :new)
                             (make-board (getf args :width)
                                         (getf args :height))))))))))

(s:define-start-function (editor) editor (:resizable T))
