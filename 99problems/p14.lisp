; P14 (*) Duplicate the elements of a list.
; Example:
; * (dupli '(a b c c d))
; (A A B B C C C C D D)

(defun dupli (L)
    (if L
        (append (cons (car L) (cons (car L) '())) (dupli (cdr L)))
        '()
    )
)
