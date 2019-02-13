; P18 (**) Extract a slice from a list.
; Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.
;
; Example:
; * (slice '(a b c d e f g h i k) 3 7)
; (C D E F G)

(load "p05.lisp")

(defun slice-acc (L start stop Acc)
    (if L
        (if (<= start 1)
            (if (<= stop 0)
                (reverse Acc)
                (slice-acc (cdr L) start (- stop 1) (cons (car L) Acc))
            )
            (slice-acc (cdr L) (- start 1) (- stop 1) Acc)
        )
        (reverse Acc)
    )
)

(defun slice (L start stop)
    (slice-acc L start stop '())
)
