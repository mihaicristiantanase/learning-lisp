; P11 (*) Modified run-length encoding.
; Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
; 
; Example:
; * (encode-modified '(a a a a b c c a a d e e e e))
; ((4 A) B (2 C) (2 A) D (4 E))
;

(load "p10.lisp")

(defun filter-encoded (L)
    ;(print L)
    (if L
        (let ((elem (car L)))
            (if (= (car elem) 1)
                (cons (car (cdr elem)) (filter-encoded (cdr L)))
                (cons elem (filter-encoded (cdr L)))
            )
        )
        '()
    )
)

(defun encode-modified (L)
    (filter-encoded (encode L))
)
