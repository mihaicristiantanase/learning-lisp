; P08 (**) Eliminate consecutive duplicates of list elements.
; If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
; 
; Example:
; * (compress '(a a a a b c c a a d e e e e))
; (A B C A D E)

(defun compress-acc (L last-head)
    (if L
        (let ((current-head (car L)))
            (if (eq current-head last-head)
                (compress-acc (cdr L) current-head)
                (cons current-head (compress-acc (cdr L) current-head))
            )
        )
        '()
    )
)

(defun compress (L)
    (compress-acc L '())
)
