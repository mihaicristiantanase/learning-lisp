8

(defvar arr '())
(push 5 arr)

(defvar x 1010) (sleep 3)
(incf x)

;;; compute the factorial

(defun natural-numbers (n)
  (loop for i from 1 to n collecting i))
(natural-numbers 3)

(defun factorial (n)
  (eval `(* ,@(natural-numbers n))))

(dotimes (n 20)
  (format t "factorial ~d = ~d~%" n (factorial n)))

;;;

(factorial 3)
(defparameter lst '())

(eval `(* ,@(natural-numbers 4)))
(eval `(* ,@(natural-numbers 3)))
(eval `(* ,@(natural-numbers 2)))

(defparameter lst '(1 2))
(push 3 (cdr (last lst)))
lst

(loop for lst on '(f l o r y   s i   m i h a i   F Y M) do
      (format t "~{~a~}~%" lst))

(defvar text "flory si mihai FYM")
(loop for lst on (loop for c across text collect c) do
      (format t "~%~{~a~}~%" lst))
