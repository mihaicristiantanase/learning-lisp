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
  (format t " ~a | ~a | ~a ~%"
          (first line)
          (second line)
          (third line)))

(defun print-board-sep ()
  (format t "~&---+---+--~%"))

(defun print-board (board)
  (let ((b (sublis '((x . "X")
                     (o . "O")
                     (nil . " ")) board)))
    (print-board-line b)
    (print-board-sep)
    (print-board-line (nthcdr 3 b))
    (print-board-sep)
    (print-board-line (nthcdr 6 b))))

(print-board '(x o o nil x nil o nil x))

;; 9.6
(defun gross-pay ()
  (format t "Please set the hourly wage ($): ")
  (let ((rate (read)))
    (format t "Please set the number of hours worked: ~%")
    (let ((hours (read)))
      (format t "Payment: $~a~%" (* rate hours)))))

(gross-pay)

;; 9.7
(defun cookie-monster ()
  (format t "Give men cookie!!!~%")
  (format t "Cookie?")
  (let ((x (read)))
    (if (equal 'cookie x)
      (format t "Thank you!...Munch munnch munch...BURP~%")
      (progn
        (format t "No want ~a...~%" x)
        (cookie-monster)))))

(cookie-monster)

(if (yes-or-no-p "Do you want to be good?")
  (format t "Than just be good!~%")
  (format t "Why are you asking, than?!~%"))

*terminal-io*

;; 9.10
(defun space-over (n)
  (cond ((< n 0) (format t "Error!"))
        ((= n 0) nil)
        ((> n 0) (format t " ") (space-over (1- n)))))

(defun test (n)
  (format t "~%>>>")
  (space-over n)
  (format t "<<<"))

(test 4)
(test 30)

(defun plot-one-point (plotting-string y-val)
  (space-over y-val)
  (format t "~a~%" plotting-string))

(plot-one-point "a" 3)
(plot-one-point ">" 3)

(defun plot-points (plotting-string y-vals)
  (when (not (null y-vals))
    (plot-one-point plotting-string (first y-vals))
    (plot-points plotting-string (rest y-vals))))

(plot-points "< >" '(4 6 8 10 8 6 4))

(defun generate (m n)
  (labels ((generate-acc (m n acc)
                         (if (> m n)
                           acc
                           (generate-acc (1+ m) n (append acc (list m))))))
    (generate-acc m n '())))

(generate -3 5)
(generate 8 5)
(generate 5 5)

(defun square (n) (* n n))

(defun make-graph ()
  (format t "Function to graph? ")
  (finish-output)
  (let ((fun (read)))
    (format t "Starting x value? ")
    (finish-output)
    (let ((x-start (read)))
      (format t "Ending x value? ")
      (finish-output)
      (let ((x-end (read)))
        (format t "Plotting string? ")
        (finish-output)
        (let ((plotting-string (read)))
          (plot-points plotting-string 
                       (mapcar fun (generate x-start x-end))))))))

(make-graph)
