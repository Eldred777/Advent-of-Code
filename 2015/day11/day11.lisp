; Throughout, we treat the password in reverse order for convenience until the
; final answer.

;; FILE IO SHENANIGANS
(defun list-to-string (lst)
  (format nil "~{~A~}" lst))

(defun password-to-string (password)
  (list-to-string (reverse password)))

(defun string-to-password (string)
  (reverse
    (loop for c across string
          collect c)))

(defun read-input-password (&optional (path "./2015/day11/input"))
  (let ((x nil)
        (in (open path :direction :input)))
    (setf x (string-to-password (read-line in)))
    (close in)
    x))


;;; Useful macros 
(defmacro enumerated-table (start &rest names)
  "Generates an enumerated association table from given names."
  (let ((index (1- start)))
    `',(loop for name in names collect
               `(,name . ,(incf index)))))

(defmacro all (&body body)
  `(not ,`(member nil (list ,@body))))

(defparameter *alphabet-indices*
              (enumerated-table 0
                a b c d e f g h i j k l m n o p q r s t u v w x y z))


;;; Incrementing passwords. 
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
       (setf (car password) 'a) ; increment first letter 
       (setf (cdr password) ; increment the rest of the letters 
         (increment-password (cdr password))))
      (setf (car password) (increment-letter (car password)))) ; else, increment first letter 
  password) ; return password 


;; part 1 predicates 
(defun p1-pred1 (init-password)
  ; FIXME: Throwing weird error, the value nil is not of type number. Input '(b h g k x b z v)
  ; Pred: increasing straight of at least 3 letters.
  (let ((result nil)
        (do ((password init-password (cdr password)))
            (; test form: check whether a match has been found, OR if password has <=2 letters
             (or (eq result t)
                 (<= (length password) 2))
             ; return result 
             result)
          (; body: process whether FIRST THREE CHARACTERS in ascending order 
           ; TODO: implement 
          ))))

  (defun p1-pred2 (password)
    ; Pred: no i, o, l
    (not (member t (loop for x in password
                         collect (if (member x '(i o l)) t nil)))))

  (defun p1-pred3 (password)
    ; Pred: Double repeat. 
    ; Following checks that there are two non-overlapping repeats
    (member t
            (cddr
              (member t
                      ; Following creates a list that is t when there is a repeat, nil else
                      (let ((previous (car password))
                            (comparison-result nil))
                        ; iterate from second element
                        (loop for x in (cdr password)
                              collect (progn
                                       (setf comparison-result (eq x previous))
                                       ; Store var for next loop 
                                       (setf previous x)
                                       comparison-result)))))))

  (defun p1-pred (password)
    (all
      (p1-pred1 password)
      (p1-pred2 password)
      (p1-pred3 password)))

  ;;; TODO Part 2

  ;;; Define main functions. 
  (defun part1 (init-password)
    ;; Increment input until it satisfies part 1 predicates. 
    ;; Output to console. 
    (format t "Part 1: ~a~%"
      (do ((password init-password (increment-password password)))
          ((p1-pred password) (password-to-string password)))))

  (defun main ()
    (let ((password (read-input-password)))
      (format T "Initial password: ~a~%" (password-to-string password))
      (part1 password)))

  (main)
