; P23 (**) Extract a given number of randomly selected elements from a list.
; The selected items shall be returned in a list.
; Example:
; * (rnd-select '(a b c d e f g h) 3)
; (E D A)
;
; Hint: Use the built-in random number generator and the result of problem P20.

(load "p20.lisp")
(load "p03.lisp")

(defun rnd-remove (L k)
    (if (= k 0)
        L
        (rnd-remove (remove-at L (random (length L))) (- k 1))
    )
)

(defun rnd-select (L k)
    (if L
        (if (= k 0)
            '()
            (let ((idx (+ (random (length L)) 1)))
                (cons (element-at L idx) (rnd-select (remove-at L idx) (- k 1)))
            )
        )
        '()
    )
)
