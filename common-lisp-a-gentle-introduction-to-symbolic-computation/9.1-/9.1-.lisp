;; 9.1

(defun 9-1-display ()
  (format t "There are old pilots,~%and there are bold pilots,~%but there are no old bold pilots."))
(9-1-display)

;; 9.2
(defun draw-line (n)
  (when (plusp n)
    (format t "*")
    (draw-line (1- n))))

(draw-line 4)
(draw-line 9)

;; 9.3
(defun draw-box (n m)
  (when (plusp m)
    (draw-line n)
    (format t "~%")
    (draw-box n (1- m))))

(draw-box 10 4)
(draw-box 10 9)

;; 9.4
(defun ninety-nine-bottles (n)
  (when (plusp n)
    (format t "~a bottles of beer on the wall,~%" n)
    (format t "~a bottles of beer!~%" n)
    (format t "You take one down,~%")
    (format t "Pass it around,~%")
    (if (> n 1)
      (format t "~a bottles of beer on the wall.~%" (1- n))
      (format t "Everybody's drunk!~%"))
    (format t "~%")
    (ninety-nine-bottles (1- n))))

(ninety-nine-bottles 3)

;; 9.5
(defun print-elem (elem)
  (cond ((null elem) (format t "   "))
        (t (format t " ~a " elem))))

(defun print-board (board)
  (print-elem (nth 0 board))
  (format t "|")
  (print-elem (nth 1 board))
  (format t "|")
  (print-elem (nth 2 board))
  (format t "~%-----------~%")
  (print-elem (nth 3 board))
  (format t "|")
  (print-elem (nth 4 board))
  (format t "|")
  (print-elem (nth 5 board))
  (format t "~%-----------~%")
  (print-elem (nth 6 board))
  (format t "|")
  (print-elem (nth 7 board))
  (format t "|")
  (print-elem (nth 8 board)))

(print-board '(x o o nil x nil o nil x))
