; Throughout, we treat the password in reverse order for convenience until the
; final answer.

;; FILE IO SHENANIGANS
(defun list-to-string (lst)
  (string-downcase (format nil "~{~a~}" lst)))

(defun password-to-string (password)
  (list-to-string (reverse password)))

(defun string-to-password (string)
  (reverse
    (loop for c across (string-upcase string)
          collect (intern (string c)))))

(defun read-input-password (&optional (path "./2015/day11/input"))
  (string-to-password (car (uiop:read-file-lines path))))
;   (let ((x nil)
;         (in-file (open path :direction :input)))
;     (setf x (string-to-password (read-line in-file)))
;     (close in-file)
;     x))

(defparameter *password* (read-input-password))

(defun enumerated-table (start list-of-names)
  "Generates an enumerated association table from given names."
  (let ((index (1- start)))
    (loop for name in list-of-names collect
            ; name)))
            `(,name . ,(incf index)))))

;;; Useful macros 
(defmacro all (&body body)
  `(not ,`(member nil (list ,@body))))

(defparameter *alphabet* '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(defparameter *alphabet-indices* (enumerated-table 0 *alphabet*))

(defparameter *alphabet-length* (length *alphabet*))

;;; Incrementing passwords. 
(defun to-index (letter)
  (cdr (assoc letter *alphabet-indices*)))

(defun to-letter (index)
  (car (rassoc index *alphabet-indices*)))

(defun increment-letter (letter)
  (to-letter (mod (1+ (to-index letter)) *alphabet-length*)))

(defun increment-password (password)
  ;; Mutates password. 
  ;; Since the password  is stored from back to front, we mutate from the front.
  (if (eq (car password) 'z)
      (progn
       (setf (car password) 'a) ; increment last letter 
       (setf (cdr password) ; increment the rest of the letters 
         (increment-password (cdr password))))
      (setf (car password) (increment-letter (car password)))) ; else, increment last letter 
  password) ; return password 

(defun 1? (x)
  (eq x 1))

;; part 1 predicates 
(defun p1-pred1 (init-password)
  ; Pred: increasing straight of at least 3 letters.
  (let ((result nil))
    (do ((password init-password (cdr password)))
        (; test form: check whether a match has been found, OR if password has <=2 letters
         (or result
             (<= (length password) 2))
         ; return result 
         result)
      (; body: process whether LAST THREE CHARACTERS in ascending order 
       ; TODO: implement 
       ; e.g. "abc"
       let ((x3 (to-index (first password)))
            (x2 (to-index (second password)))
            (x1 (to-index (third password))))
        (setf result
          (and
           (1? (- x3 x2))
           (1? (- x2 x1))))))))

; (p1-pred1 (read-input-password))
; (p1-pred1 '(e d c b h g k x a b d z v))

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

;;; Define main functions. 
(defun part1 (init-password)
  ;; Increment input until it satisfies part 1 predicates. 
  ;; Return password (in reverse form)
  (do ((password init-password (increment-password password)))
      ((p1-pred password)
       password)))

(defun part2 (init-password)
  ;; Increment input until it satisfies part 1 predicates. 
  ;; Return password (in reverse form)
  (do ((password (increment-password (part1 init-password))
                 (increment-password password)))
      ((p1-pred password)
       password)))

(defun main ()
  (let ((password (read-input-password)))
    (format T "Initial password: ~a~%" (password-to-string password))
    (format t "Part 1: ~a~%" (password-to-string (part1 password)))
    (format t "Part 2: ~a~%" (password-to-string (part2 password)))))

(main)
