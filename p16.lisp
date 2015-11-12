; P16 (**) Drop every N'th element from a list.
; Example:
; * (drop '(a b c d e f g h i k) 3)
; (A B D E G H K)

(defun drop-idx (L n cidx)
    (if L
        (if (= n cidx)
            (drop-idx (cdr L) n 1)
            (cons (car L) (drop-idx (cdr L) n (+ cidx 1)))
        )
        '()
    )
)

(defun drop (L n)
    (drop-idx L n 1)
)
