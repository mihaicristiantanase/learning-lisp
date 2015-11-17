; P20 (*) Remove the K'th element from a list.
; Example:
; * (remove-at '(a b c d) 2)
; (A C D)

(defun remove-at (L k)
    (if L
        (if (= k 1)
          (cdr L)
          (cons (car L) (remove-at (cdr L) (- k 1)))
        )
        '()
    )
)
