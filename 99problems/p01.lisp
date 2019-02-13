; P01 (*) Find the last box of a list.
; Example:
; * (my-last '(a b c d))
; (D)


(defun my-last (L)
  (if L
    (if (cdr L)
      (my-last (cdr L))
      (car L)
    )
    '()
  )
)
