; P07 (**) Flatten a nested list structure.
; Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
; 
; Example:
; * (my-flatten '(a (b (c d) e)))
; (A B C D E)
; 
; Hint: Use the predefined functions list and append.
;
; (cons a (b (c d) e))
; (cons a (cons b ((c d) e)))
; (cons a (cons b (cons (c d) (e))))

(defun my-flatten (L)
    (if L
        (if (listp (car L))
            (append (my-flatten (car L)) (my-flatten (cdr L)))
            (cons (car L) (my-flatten (cdr L)))
        )
        '()
    )
)
