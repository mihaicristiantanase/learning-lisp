; P10 (*) Run-length encoding of a list.
; Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
; 
; Example:
; * (encode '(a a a a b c c a a d e e e e))
; ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
;
; TODO :fix pack reversing the list

(load "p09.lisp")

(defun encode-packed (L)
    (if L
        (let ((head (car (car L))))
            (cons (cons (length (car L)) (cons head '())) (encode-packed (cdr L)))
        )
        '()
    )
)

(defun encode (L)
    (encode-packed (pack L))
)
