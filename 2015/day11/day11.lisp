; Throughout, we treat the password in reverse order for convenience until the
; final answer.
(defun read-input ()
  (let ((x "nil") (in (open "./2015/day11/input")))
    (setf x (loop for c across (read-line in)
                  collect c))
    (close in)
    (reverse x)))

(defmacro enumerated-table (start &rest names)
  "Generates an enumerated association table from given names."
  (let ((index (1- start)))
    `',(loop for name in names collect
               `(,name . ,(incf index)))))

(defparameter *alphabet-indices*
              (enumerated-table 0
                a b c d e f g h i j k l m n o p q r s t u v w x y z))

(defun to-index (letter)
  (cdr (assoc letter *alphabet-indices*)))

(defun to-letter (index)
  (car (rassoc index *alphabet-indices*)))

(defun increment-letter (letter)
  (to-letter (mod (1+ (to-index letter)) 26)))

(defun increment-password (password)
  ;; Mutates password. 
  (if (eq (car password) 'z)
      (progn
       (setf (car password) 'a)
       (setf (cdr password) (increment-password (cdr password))))
      (setf (car password) (increment-letter (car password))))
  password)

;; part 1 predicates 
(defmacro all (&body body)
  `(not ,`(member nil (list ,@body))))
(defmacro any (&body body)
  `(member t ,`(list ,@body)))

; (defun p1-pred1 (password)
;   (let ((alpha (caddr password))
;         (beta (cadr password))
;         (gamma (car password)))
;     (all
;       (list
;        (if (= (- (to-index beta)
;                  (to-index alpha))
;              1) t nil)
;        (if (= (- (to-index gamma)
;                  (to-index beta))
;              1) t nil)))))
(defun p1-pred1 (password)
  ;;; PRECONDITION: password length at least 3
  ; (format T "~a~%" password)
  (let ((previous1 (car password))
        (previous2 (cadr password))
        (comparison-result nil))
    (member t
      (loop for x in (cddr password) ; iterate from second element
            collect (progn
                     (setf comparison-result (all
                                               (= 1 (- (to-index previous1)
                                                       (to-index previous2)))
                                               (= 1 (- (to-index previous2)
                                                       (to-index x))))) ; store result of comparison
                    ;  (format T "~a ~a ~a :: ~a ~%" previous1 previous2 x comparison-result)
                     (setf previous1 previous2) ; store var for next loop
                     (setf previous2 x) ; store var for next loop 
                     comparison-result ; collect result of comparison
                                    )))))

(defun p1-pred2 (password)
  (member t (loop for x in password
                  collect (if (member x '(i o l)) t nil))))

; (defun p1-pred3-helper (password)
;   (list (eq (car password) (cadr password))
;         (p1-pred3-helper (cdr password))))

(defun p1-pred3 (password)
  (>= (count t
          (let ((previous (car password))
                (comparison-result nil))
            (loop for x in (cdr password) ; iterate from second element
                  collect (progn
                           (setf comparison-result (eq x previous)) ; store result of comparison
                           (setf previous x) ; store var for next loop 
                           comparison-result ; collect result of comparison
                                    ))))
      2))

(defun p1-pred (password)
  (all
    (p1-pred1 password)
    (p1-pred2 password)
    (p1-pred3 password)))

(defun part1 ()
  (p1-pred (read-input)))

(defun main ()
  (part1))
