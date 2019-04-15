;;; Compares features and returns the number of common ones
;;; Ex: (large red shiny cube -vs- small shiny red four-sided pyramid)
;;; Returns: (2 COMMON FEATURES)

(defun right-side (x)
  (cdr (member '-vs- x)))

(defun left-side (x)
  (right-side (reverse x)))

(defun count-common (l r)
  (length (intersection l r)))

(defun compare (x)
  (list (count-common (left-side x) (right-side x)) 'common 'features))

(defvar l)
(setf l '(large red shiny cube -vs- small shiny red four-sided pyramid))
(right-side l)
(right-side '(mihai -vs- flory))
(left-side l)
(left-side '(mihai -vs- flory))
(count-common '(mihai flory) '(flory))
(count-common '(mihai) '(flory))
(compare l)
(compare '(small red metal cube -vs- red plastic small cube))
