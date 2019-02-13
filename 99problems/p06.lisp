; P06 (*) Find out whether a list is a palindrome.
; A palindrome can be read forward or backward; e.g. (x a m a x).

(defun equal-lists (L1 L2)
    (if L1
        (if L2
            (if (eq (car L1) (car L2))
                (equal-lists (cdr L1) (cdr L2))
                '()
            )
            '()
        )
        (if L2 '() t)
    )
)

(load "p05.lisp")

(defun palindrome (L)
    ;(equal L (rev L))
    (equal-lists L (rev L))
)
