; P17 (*) Split a list into two parts; the length of the first part is given.
; Do not use any predefined predicates.
;
; Example:
; * (split '(a b c d e f g h i k) 3)
; ( (A B C) (D E F G H I K))

(load "p05.lisp")

(defun split-acc (L n Acc)
    (if L
        (if (<= n 0)
            (if Acc
                (cons (reverse Acc) (list L))
                L
            )
            (split-acc (cdr L) (- n 1) (cons (car L) Acc))
        )
        (reverse Acc)
    )
)

; (defun split (L n)
;     (if L
;         (if (= n 1)
;             (append (cons (cons (car L) '()) '()) (cons (cdr L) '()))
;             (let ((rec (split L (- n 1))))
;                 (print rec)
;                 (print L)
;                 (cons (cons (car L) (car rec)) (cdr rec))
;             )
;         )
;         '()
;     )
; )

(defun split (L n)
    (split-acc L n '())
)
