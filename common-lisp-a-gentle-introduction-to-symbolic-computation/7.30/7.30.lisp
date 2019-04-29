;;; Triangular dictinoary

(defvar words
  '((one un)
    (two deux)
    (three trois)
    (four quatre)
    (five cinq)))

(defvar spanish-words '(uno dos tres quatro cinco))

(defun tri-dict ()
  (mapcar #'(lambda (w sw) (append w (list sw)))
          words spanish-words))

(format t "~{~a~%~}" (tri-dict))
