(defvar *fn*)
(setf *fn* #'cons)

(funcall *fn* 'a 'b)

(defun zip (x y)
  (if (null x)
    '()
    (append (list (funcall *fn* (first x) (first y))) (zip (rest x) (rest y)))))

(zip '(a b c) '(1 2 3))

#'if
'if
#'turnisp
'turnisp
