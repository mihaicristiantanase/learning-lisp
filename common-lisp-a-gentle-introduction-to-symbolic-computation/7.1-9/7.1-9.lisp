;; 7.1
(defun add1 (x)
  (+ x 1))
(mapcar #'add1 '(1 3 5 7 9))

;; 7.2
(defvar daily-planet
  '((olsen jimmy 123-76-4535 cub-reporter)
    (kent clark 089-52-6787 reporter)
    (lane lois 951-26-1438 reporter)
    (white perry 355-16-7439 editor)))
(mapcar #'third daily-planet)

;; 7.3
(mapcar #'zerop '(2 0 3 4 0 -5 -6))

;; 7.4 
(defun greater-than-five-p (x) (> x 5))
(mapcar #'greater-than-five-p '(2 0 3 4 0 6 -6))

;; 7.5
(funcall (lambda (n) (- n 7)) 9)

;; 7.6
(funcall (lambda (x) (if (equal x t) t nil)) t)

;; 7.7
(mapcar (lambda (x) (if (equal x 'up) 'down 'up))
        '(up down up up))

;; 7.8
(defun xk (x k)
  (find-if #'(lambda (x) (and (> x (- k 10)) (< x (+ k 10))))
           x))
(xk '(1 3 -20 9 8 10) 11)

;; 7.9
(defun find-nested (l)
  (find-if #'(lambda (x) (and (listp x) (> (length x) 0))) l))
(find-nested '(1 2 3 '(9 9) 4))
