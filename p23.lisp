; P23 (**) Extract a given number of randomly selected elements from a list.
; The selected items shall be returned in a list.
; Example:
; * (rnd-select '(a b c d e f g h) 3)
; (E D A)
;
; Hint: Use the built-in random number generator and the result of problem P20.

(load "p20.lisp")

(defun rnd-remove (L k)
    (if (= k 0)
        L
        (rnd-remove (remove-at L (+ (random (length L)) 1)) (- k 1))
    )
)

(defun rnd-select (L k)
    (rnd-remove L (max (- (length L) k) 0))
)
