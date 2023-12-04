(require 'uiop)

(defparameter *characters* nil)

(with-open-file (infile "2023/day3/input")
  (progn
   (loop for line = (read-line infile nil)
         while line
         do (loop for c across line
                  do (setf *characters* (adjoin c *characters*))))
   (format t "*characters* = (~{ ~a ~})" *characters*)))
