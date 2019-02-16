(defun interpolation (f x from to)
  (let ((v (car (mapcar f (list x)))))
    (+ (* to v) (* from (- 1.0 v)))))

(defun linear (x)
  x)

(defun easein (x)
  (* x x))

(defun easein-2 (x)
  (* x x x))

(defun easein-3 (x)
  (* x x x x))

(defun intersin (x)
  (sin x))

(defun decelerate (x)
  (- 1 (expt (- 1 x) 2)))

(defun accelerate-decelerate (x)
  (+ (/ (cos (* (+ x 1) pi)) 2) 0.5))

(defun test-interpolation (f x from to)
  (if (< x 1.0)
    (progn
      (let ((res (interpolation f x from to)))
        (format t "~3,2F~%" res))
      (test-interpolation f (+ x 0.02) from to)
      )))

(test-interpolation #'accelerate-decelerate 0 8 4)

(write-line "")
