;; 7.11
(defun greater-than-1-and-less-than-5 (l)
  (remove-if-not #'(lambda (x) (< 1 x 5)) l))

(greater-than-1-and-less-than-5 '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17
                                  18 19 20 21 22 23 24 25 26 27 28 29))

;; 7.12
(defun the-count (l)
  (length (remove-if-not #'(lambda (x) (equal 'the x)) l)))
(the-count '(I am here with the one and the only one))

;; 7.13
(defun extract-lists-of-2 (l)
  (remove-if-not #'(lambda (subl) (equal 2 (length subl))) l))
(extract-lists-of-2 '((1 2) (do re mi) (fa so) (flory mihai) ()))

;; 7.14
(defun r-intersection (x y)
  (remove-if-not #'(lambda (elem) (member elem y)) x))
(r-intersection '(1 2 3 4) '(3 4 5 6))
(r-intersection '(1 2) '(3 4 5 6))
(r-intersection '(1 2) '(3 4 1 6))

(defun r-union (x y)
  (append x (remove-if #'(lambda (elem) (member elem x)) y)))
(r-union '(1 2 3 4) '(3 4 5 6))
(r-union '(1 2) '(3 4 5 6))
(r-union '(1 2) '(3 4 1 6))
