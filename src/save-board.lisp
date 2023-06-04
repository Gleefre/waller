(in-package #:waller)

(defun board-plist (board)
  (list :cells (cells board)
        :states (alexandria:hash-table-alist (states board))
        :positions (alexandria:hash-table-alist (positions board))))

(defun plist-board (plist)
  (make-instance 'board
                 :cells (getf plist :cells)
                 :states (alexandria:alist-hash-table (getf plist :states) :test 'equal)
                 :positions (alexandria:alist-hash-table (getf plist :positions) :test 'equal)))

(defun save-board (board filename &optional (if-exists :supersede))
  (alexandria:with-output-to-file (out filename :if-exists if-exists)
    (let ((*print-readably* T))
      (print (board-plist board) out))))

(defun load-board (file)
  (plist-board (uiop:read-file-form file)))
