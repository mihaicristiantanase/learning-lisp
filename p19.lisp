; P19 (**) Rotate a list N places to the left.
; Examples:
; * (rotate '(a b c d e f g h) 3)
; (D E F G H A B C)
;
; * (rotate '(a b c d e f g h) -2)
; (G H A B C D E F)
;
; Hint: Use the predefined functions length and append, as well as the result of problem P17.

(load "p17.lisp")

(defun rotate (L n)
    (let ((l_split (split L (mod n (length L)))))
        (print l_split)
        (append (car (cdr l_split)) (car l_split))
    )
)
