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
(defun print-board-line (line)
  (format t "~a | ~a | ~a ~%"
          (first line)
          (second line)
          (third line)))

(defun print-board-sep ()
  (format t "~&---------~%"))

(defun print-board (board)
  (let ((b (sublis '((x . " X ")
                     (o . " O ")
                     (nil . "   ")) board)))
    (print-board-line b)
    (print-board-sep)
    (print-board-line (nthcdr 3 b))
    (print-board-sep)
    (print-board-line (nthcdr 6 b))))

(print-board '(x o o nil x nil o nil x))
