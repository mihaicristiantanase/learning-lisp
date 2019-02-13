; P21 (*) Insert an element at a given position into a list.
; Example:
; * (insert-at 'alfa '(a b c d) 2)
; (A ALFA B C D)

(defun insert-at (elem L k)
    (if L
        (if (= k 1)
            (cons elem L)
            (cons (car L) (insert-at elem (cdr L) (- k 1)))
        )
        '()
    )
)
