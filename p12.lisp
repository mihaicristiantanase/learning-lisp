; P12 (**) Decode a run-length encoded list.
; Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.
; (decode-runlength '((4 A) B (2 C) (2 A) D (4 E)))

(defun ls (name nr)
    (if (> nr 0)
        (progn
            (print name)
            (ls name (- nr 1))
        )
        0
    )
)

(defun build-list (c n)
    (if (= n 0)
        '()
        (cons c (build-list c (- n 1)))
    )
)

(defun decode-runlength (L)
    (if L
        (let ((elem (car L)))
            (if (listp elem)
                (append (build-list (car (cdr elem)) (car elem)) (decode-runlength (cdr L)))
                (cons elem (decode-runlength (cdr L)))
            )
        )
        '()
    )
)
