; P04 (*) Find the number of elements of a list.

(defun list-len (L)
    (if L
        (+ 1 (list-len (cdr L)))
        0
    )
)
