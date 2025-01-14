(require :uiop)
(require :split-sequence)

(defun sum (input-list)
  (reduce #'+ input-list))

(defun parse-integer-then-sum (input-list)
  (reduce #'+
      (mapcar #'parse-integer input-list)))

(defun main ()
  ;; Process: 
  ;; 1) Read text file into a list of strings, each containing one line of the original file 
  ;;    -> https://stackoverflow.com/a/48185783/12133378
  ;; 2) Split the list into lists of numbers by elf and reduce with + 
  ;;    -> https://www.reddit.com/r/lisp/comments/m5grm5/split_list_into_sublists/gqzw8p2/ 
  ;; 3) Find top 3 and max, format to console
  (let ((result-list
         (sort
             (mapcar #'parse-integer-then-sum
                 (split-sequence:split-sequence ""
                                                (uiop:read-file-lines "2022/day1/input")
                                                :test #'equal))
             #'>)))
    (format T "Largest element ~d~%" (first result-list))
    (format t "Largest three elements ~d" (+ (first result-list)
                                             (second result-list)
                                             (third result-list)))))
(main)
