; P09 (**) Pack consecutive duplicates of list elements into sublists.
; If a list contains repeated elements they should be placed in separate sublists.
; 
; Example:
; * (pack '(a a a a b c c a a d e e e e))
; ((A A A A) (B) (C C) (A A) (D) (E E E E))
;

; laod this for reverse
(load "p05.lisp")

(defun pack-acc (L L-result L-current)
    (if L
        (let ((current-head (car L))
              (last-head (car L-current)))
            ;(print "-------------------------")
            ;(print current-head)
            ;(print last-head)
            ;(print L-result)
            ;(print L-current)
            (if last-head
                (if (eq current-head last-head)
                    (pack-acc (cdr L) L-result (cons current-head L-current))
                    (pack-acc (cdr L) (cons L-current L-result) (cons current-head '()))
                )
                (pack-acc (cdr L) L-result (cons current-head L-current))
            )
        )
        (if L-current
            (cons L-current L-result)
            L-result
        )
    )
)

(defun pack (L)
    (reverse (pack-acc L '() '()))
)
