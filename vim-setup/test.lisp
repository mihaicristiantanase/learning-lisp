(sin 0.3)

(* 1 3)

(defun mihai (x)
  (sin x))

(mihai 0.5)

(defvar *db* nil)
(setf *db* (list :name "mihai" :rate 10))

*db*

(setf (getf *db* :name) "flory")

(setf (getf *db* :name) "cucu")
