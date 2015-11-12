; P05 (*) Reverse a list.
;
; (1 2 3 4)
;

; this solution is based on accumulation
; TODO: find a one-function solution
(defun rev-acc (L Lacc)
    (if L
        (rev-acc (cdr L) (cons (car L) Lacc))
        Lacc
    )
)

(defun rev (L)
    (rev-acc L '())
)
