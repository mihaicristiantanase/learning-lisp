; P02 (*) Find the last but one box of a list.
; Example:
; * (my-but-last '(a b c d))
; (C D)

(defun my-but-last (L)
    (if L
        (if (cdr L)
            (if (cdr (cdr L))
                (my-but-last (cdr L))
                (car L)
            )
            '()
        )
        '()
    )
)
