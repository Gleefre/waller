(in-package #:waller)

(defun clean-cell (cell)
  (loop for (thing state) on cell by #'cddr
        when state
        collect thing and collect state))

(defun clean-cells (cells)
  (destructuring-bind (w h) (array-dimensions cells)
    (let ((clean-cells (make-array (list w h))))
      (dotimes (x w clean-cells)
        (dotimes (y w)
          (setf (aref clean-cells x y)
                (clean-cell (aref cells x y))))))))

(defun board-plist (board)
  (list :cells (clean-cells (cells board))
        :states (alexandria:hash-table-alist (states board))
        :positions (alexandria:hash-table-alist (positions board))
        :description (board-description board)))

(defun plist-board (plist)
  (make-instance 'board
                 :cells (getf plist :cells)
                 :states (alexandria:alist-hash-table (getf plist :states) :test 'equal)
                 :positions (alexandria:alist-hash-table (getf plist :positions) :test 'equal)
                 :description (or (getf plist :description) "")))

(defun save-board (board filename &optional (if-exists :supersede))
  (alexandria:with-output-to-file (out filename :if-exists if-exists)
    (let ((*print-readably* T))
      (print (board-plist board) out))))

(defun load-board (file)
  (plist-board (uiop:read-file-form file)))
