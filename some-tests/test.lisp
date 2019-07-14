(sin 0.3)

(* 1 3)

(defun mihai (x)
  (sin x))

(mihai 0.5)

(defvar *db* nil)

*db*

(setf (getf *db* :name) "flory")

(setf (getf *db* :name) "cucu")

(defun add-user (name rate)
  (push (list :name name :rate rate) *db*))

(add-user "mihai" 9)
(add-user "flory" 10)
(add-user "costi" 10)

(format t "~{~a:~10t~%~}" *db*)

(list :mihai 10)

(format t "~a:~10t~a~%" :mihai 10)

(describe '(1 2 3))

(let ((f '(cos 0)))
  (eval f))

(defun my-2-last (L)
  (if L
    (if (cdr L)
      (if (cdr (cdr L))
        (my-2-last (cdr L))
        L
        )
      '())
    '()))
(my-2-last '(1 2 3 4))

(defun fibbo (n)
  (if (< n 2)
    1
    (+ (fibbo (- n 1)) (fibbo (- n 2)))))

(defun fibbo-improved-rec (accumulated n)
  (if (< n 1)
    accumulated
    (let ((last-2 (my-2-last accumulated)))
          (let ((val (+ (car last-2) (car (cdr last-2)))))
      (fibbo-improved-rec (append accumulated (list val)) (- n 1))))))
(defun fibbo-improved (n)
  (car (cdr (my-2-last (fibbo-improved-rec '(0 1) n)))))
(fibbo-improved 19)

(let ((all (fibbo-improved 40)))
  (format t "~{~d ~}" all))

(time (fibbo 40))

(time (fibbo-improved 40))

(cons 8 '(1 2 3))

(defun fibbo-improved-rec2 (accumulated n)
  (if (< n 1)
    accumulated
    (let ((last (car accumulated)) (prevlast (car (cdr accumulated))))
      (let ((val (+ last prevlast)))
        (fibbo-improved-rec2 (cons val accumulated) (- n 1))))))
(defun fibbo-improved2 (n)
  "Simpler fibbonacci generator"
  (car (fibbo-improved-rec2 '(0 1) n)))
(fibbo-improved2 4)

(documentation 'fibbo-improved2 'function)

(describe 'fibbo-improved2)

(format t "mihai is here!~%")

(defun plot (f &optional (minx 0) (maxx 10) (step 1))
  (loop for x from minx below maxx by step do
        (loop repeat (funcall f x) do (format t "#"))
        (format t "~%")))
(plot #'(lambda (x) (* x 7)))

(defparameter x 0.5)
x
(setf x 10)

(defparameter *lst* '())
(loop repeat 100 do (push (random 100) *lst*))
(dolist (x *lst*)
  (format t "~d " x))

(defparameter *array* (make-array '(2)))
(loop repeat 10 do
      (incf (aref *array* (random (length *array*)))))
(loop for i from 0 below (length *array*) by 1 do
      (format t "~d~%" (aref *array* i)))

(defparameter x 1)
(defparameter y 2)
(rotatef x y)
(format t "~d ~d" x y)
