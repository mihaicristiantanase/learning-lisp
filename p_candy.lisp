; Children with hiher priority get more candies then the ones with lower priority, next to them
; (give-candy '(4 3 2 8))
; result: (3 2 1 2)
;
; '()
; '((1 3))
; '( (1 2) (1 3) ) --> '( (1 2) (2 3) )
; '( (1 1) (1 2) (2 3) )
; '( (1 2) (1 1) (1 2) (2 3) ) --> '( (2 2) (1 1) (1 2) (2 3) )

(load "p05.lisp")

(defun fix-priority (L)
    ; (print "fix-priority")
    ; (print L)
    (if (and L (cdr L))
        (let ((val1 (car L))
              (val2 (car (cdr L))))
            (let ((val1_candy (car val1))
                  (val2_candy (car val2))
                  (val1_prio (car (cdr val1)))
                  (val2_prio (car (cdr val2))))

                ; (print "Comparing: ")
                ; (print val1_candy)
                ; (print val2_candy)
                ; (print val1_prio)
                ; (print val2_prio)

                (if (and (> val1_prio val2_prio) (<= val1_candy val2_candy))
                    (cons (list (+ val2_candy 1) val1_prio) (cdr L))
                    (if (and (< val1_prio val2_prio) (>= val1_candy val2_candy))
                        (cons val1 (fix-priority (cons (list (+ val1_candy 1) val2_prio)  (cdr (cdr L)))))
                        (if (and (= val1_prio val2_prio) (not (= val1_candy val2_candy)))
                            (let ((val_max (list (max val1_candy val2_candy) val1_prio)))
                                (cons val_max (fix-priority (cons val_max  (cdr (cdr L)))))
                            )
                            L
                        )
                    )
                )
            )
        )
        L
    )
)

(defun extract-candy-from (L)
    (if L
        (cons (car (car L)) (extract-candy-from (cdr L)))
        '()
    )
)

(defun give-candy-acc (L Acc)
    (if L
        (let ((l-head (car L)))
            (give-candy-acc (cdr L) (fix-priority (cons (list 1 l-head) Acc)))
        )
        (reverse (extract-candy-from Acc))
    )
)

(defun give-candy (L)
    (give-candy-acc L '())
)
