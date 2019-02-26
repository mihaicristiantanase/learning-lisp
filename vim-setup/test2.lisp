8

(defvar arr '())
(push 5 arr)

(defvar x 1010)
(sleep 3)
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
