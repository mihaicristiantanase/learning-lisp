;;; Transpose a song from one key to another

(defvar *note-table*
  '((c       . 1)
    (c-sharp . 2)
    (d       . 3)
    (d-sharp . 4)
    (e       . 5)
    (f       . 6)
    (f-sharp . 7)
    (g       . 8)
    (g-sharp . 9)
    (a       . 10)
    (a-sharp . 11)
    (b       . 12)))

(defun numbers (notes)
  (mapcar (lambda (n) (cdr (assoc n *note-table*))) notes))

; OBS(mihai): this one looks much better, but does not return NIL for invalid
; value, for example (numbers '(5 3 1))
;(defun numbers (notes)
;  (sublis *note-table* notes))

(defun notes (numbers)
  (mapcar (lambda (n) (car (rassoc n *note-table*))) numbers))

(defun raise (halfsteps numbers)
  (mapcar (lambda (n) (+ halfsteps n)) numbers))

(defun normalize (numbers)
  (mapcar (lambda (n)
            (cond ((> n 12) (- n 12))
                  ((< n 1) (+ n 12))
                  (t n)))
          numbers))

(defun transpose (n song)
  (notes (normalize (raise n (numbers song)))))

(numbers '(e d c d e e e))

(notes '(5 3 1 3 5 5 5))

(raise 5 '(5 3 1 3 5 5 5))

(normalize '(6 10 14 13 -1 0))

(notes (notes '(5 3 1)))
(numbers (numbers '(e d c)))

(transpose 5 '(e d c d e e e))

(transpose 11 '(e d c d e e e))

(transpose 12 '(e d c d e e e))

(transpose -1 '(e d c d e e e))
