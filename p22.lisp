; P22 (*) Create a list containing all integers within a given range.
; If first argument is smaller than second, produce a list in decreasing order.
; Example:
; * (range 4 9)
; (4 5 6 7 8 9)

(defun range (start stop)
    (if (= start stop)
        (list start)
        (if (> start stop)
            '()
            (cons start (range (+ start 1) stop))
        )
    )
)
