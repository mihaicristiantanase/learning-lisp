(defun generate (n)
  "Documentation for generate with parameters n"
  (append (reverse (loop for x to n collect (/ x (- (float n)))))
          (loop for x from 1 to n collect (/ x (float n)))))

(defun process (x flat1 flat2 &optional (max-value 0.3))
  "Documentation for stuff with parameters flat"
  (mapcar #'(lambda (i)
              (let ((i (abs i)))
                (cond ((<= i flat1) 0)
                      ((>= i flat2) max-value)
                      (t (* (/ max-value (- flat2 flat1)) (- i flat1))))))
          x))

(defun print-to-calc (x)
  "Documentation for print-to-calc with parameters x"
  (format t "~&[~{~a~^, ~}]" x))

(defun test (n)
  "Documentation for test with parameters n"
  (let ((x (generate n)))
    (print-to-calc (loop for i below (length x) collect i))
    (print-to-calc (mapcar #'abs x))
    (print-to-calc (process x 0.1 0.3))))
