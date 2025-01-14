(defparameter *input* (with-open-file (file #P"2015/day1/input")
                        (read-line file)))

(defparameter *test-input* "(())))")

(defun part1 (string)
  (let ((current-floor 0))
    (loop :for char :across string
          :collect
          (list
           (if (eq char #\()
               (setf current-floor (+ current-floor 1))
               (if (eq char #\))
                   (setf current-floor (- current-floor 1))))))
    (with-open-file (output-file "2015/day1/output.txt"
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (format output-file "Current floor: ~d~%" current-floor))
    (format t "Current floor: ~d~%" current-floor)))

(part1 *test-input*) ; -2 

(defun part2 (string)
  (let ((current-floor 0)
        (index 0)
        (desired-index 0))
    (loop :for char :across string
          :collect
          (list (if (eq char #\()
                    (setf current-floor (+ current-floor 1))
                    (if (eq char #\))
                        (setf current-floor (- current-floor 1))))
                (setf index (+ index 1))
                (if (eq current-floor -1)
                    (setf desired-index index))))
    (with-open-file (output-file "2015/day1/output.txt"
                                 :direction :output
                                 :if-exists :append
                                 :if-does-not-exist :create)
      (format output-file "First index of floor -1: ~d~%" desired-index))
    (format t "First index of floor -1: ~d~%" desired-index)))

(part2 *test-input*) ; -2 

(defun main (string)
  (part1 string)
  (part2 string))

(main *test-input*)
(main *input*)
