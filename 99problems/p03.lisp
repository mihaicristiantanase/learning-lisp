; P03 (*) Find the K'th element of a list.
; The first element in the list is number 1.
; Example:
; * (element-at '(a b c d e) 3)
; C

(defun element-at (L pos)
    (if (= pos 1)
        (car L)
        (if (> pos 1)
            (element-at (cdr L) (- pos 1))
            '()
        )
    )
)
