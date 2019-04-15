;; there must be a better way of doing this!
(defun swap-first-last (l)
  (let ((fi (first l))
        (la (car (last l))))
    (cons la (append (reverse (rest (reverse (rest l)))) (list fi)))))

;; looks much better :)
(defun swap-first-last2 (l)
  (let* ((a (reverse (rest l)))
         (b (rest (reverse a))))
    (cons (first a) (append b (list (first l))))))

(defun rotate-left (l)
  (append (cdr l) (list (car l))))

(defun rotate-right (l)
  (let ((prefix (reverse (cdr (reverse l))))
        (la (car (last l))))
    (cons la prefix)))

(swap-first-last '(you cant buy love))
(swap-first-last2 '(you cant buy love))

(rotate-left '(a b c d e f))
(rotate-right '(a b c d e f))
