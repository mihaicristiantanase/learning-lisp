(defun run (f n x)
  (if (> n 0)
    (progn
      (format t "~F~%" x)
      (let ((x (car (mapcar f (list x)))))
        (run f (- n 1) x)))))

(defun sin-cos (x)
  (sin (cos x)))

(defun cos-sin (x)
  (cos (sin x)))

(run #'cos-sin 200 1)
