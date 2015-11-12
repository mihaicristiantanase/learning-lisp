; P15 (**) Replicate the elements of a list a given number of times.
; Example:
; * (repli '(a b c) 3)
; (A A A B B B C C C)

(load "p12.lisp") 

(defun repli (L n)
    (if L
        (append (build-list (car L) n) (repli (cdr L) n))
        '()
    )
)
