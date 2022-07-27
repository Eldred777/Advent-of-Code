(defun read-input ()
  (let ((x "nil") (in (open "./2015/day10/input")))
    (setf x (loop for c across (read-line in)
                  collect (digit-char-p c)))
    (close in)
    x))

(defparameter *input* (read-input))

(defun neq (a b)
  (not (eq a b)))

(defun iterate (input-list)
  (let ((output-list (list (car input-list) 0))
        (number (car input-list)))
    (do ((num-list input-list (cdr num-list)))
        ((null num-list) (reverse output-list)) ; end when num-list is nil  
      (if (neq number (car num-list))
          (progn
           (setf number (car num-list))
           (push 1 output-list)
           (push number output-list))
          (incf (second output-list))))))

(defun compute-n-iterations (n initial-value)
  (dotimes (temp n)
    (setf
      initial-value
      (iterate initial-value)))
  initial-value)

(defun compute-part1 ()
  (length (compute-n-iterations 40 *input*))) ; return length of x 

(defun part1 ()
  (format T "Part 1: ~a~%" (compute-part1)))

(defun compute-part2 ()
  (length (compute-n-iterations 50 *input*))) ; return length of x 

(defun part2 ()
  (format T "Part 2: ~a~%" (compute-part2)))

(defun main ()
  (part1)
  (part2))

(require 'trivial-benchmark)
; (benchmark:with-timing (10) (main))
(main)
