; P13 (**) Run-length encoding of a list (direct solution).
; Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
;
; Example:
; * (encode-direct '(a a a a b c c a a d e e e e))
; ((4 A) B (2 C) (2 A) D (4 E))

(load "p05.lisp")

(defun simplify-head (L)
    (if L
        (let ((head (car L)))
            (if (listp head)
                (if (= (car head) 1)
                    (cons (car (cdr head)) (cdr L))
                    (cons head (cdr L))
                )
                (cons head (cdr L))
            )
        )
        L
    )
)

(defun encode-direct-acc (L Acc)
    ;(print L)
    ;(print Acc)
    (if L
        (let ((l_head (car L))
              (acc_head_nr (car (car Acc)))
              (acc_head_val (car (cdr (car Acc)))))
            (if (eq l_head acc_head_val)
                (if (numberp acc_head_nr)
                    (encode-direct-acc (cdr L) (cons (cons (+ acc_head_nr 1) (cons l_head '())) (cdr Acc)))
                    (encode-direct-acc (cdr L) (cons (cons 1 (cons l_head '())) (cdr Acc)))
                )
                (encode-direct-acc (cdr L) (cons (cons 1 (cons l_head '())) (simplify-head Acc)))
            )
        )
        (reverse (simplify-head Acc))
    )
)

(defun encode-direct (L)
    (encode-direct-acc L '())
)
