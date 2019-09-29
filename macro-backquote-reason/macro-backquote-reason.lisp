;; The reason backquote is needed

(print "mihai")

(defmacro setq2 (v1 v2 e)
  (let ((tempvar (gensym)))
    `(let ((,tempvar ,e))
       (progn (setq ,v1 ,tempvar))
       (progn (setq ,v2 ,tempvar)))))

(defmacro setq2-list (v1 v2 e)
  (let ((tempvar (gensym)))
    (list 'let ((tempvar e)))
    `(let ((,tempvar ,e))
       (progn (setq ,v1 ,tempvar))
       (progn (setq ,v2 ,tempvar)))))

(defvar a 10)
(defvar b 20)
(setq2 a b (+ a b))

(do ((x 0 (1+ x)))
    ((> x 10) t)
    (format t "~&~d" x))

(do ((temp-one 1 (1+ temp-one))
     (temp-two 0 (1- temp-two)))
    ((> (- temp-one temp-two) 5) temp-one))

(defmacro for-swift (idx sym start range stop)
  `(dotimes )
  `(progn
     (format t "~&idx = ~a" ',idx)
     (format t "~&sym = ~a" ,sym)
     (format t "~&start = ~d" ,start)
     (format t "~&range = ~a" ',range)
     (format t "~&stop = ~d" ,stop)))

(for-swift i :in 0 - 3)

(defun for-swift (start stop)
  (do ((i start (1+ i)))
      ((> i stop))
      (print i)))

(for-swift 4 9)

(defvar my-list '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
(setf (second my-list) 9)
