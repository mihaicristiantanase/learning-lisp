;;; Tic-Tac-Toe game

(defun make-board () (list 'board 0 0 0 0 0 0 0 0 0))

(defvar *opponent* 1)
(defvar *computer* 10)
(defvar *corners* '(1 3 7 9))
(defvar *sides*
  '((1 2 3) (3 6 9) (7 8 9) (1 4 7)))
(defvar *diagonals*
  '((1 5 9) (3 5 7)))

(defvar *triplets*
  '((1 2 3) (4 5 6) (7 8 9)
    (1 4 7) (2 5 8) (3 6 9)
    (1 5 9) (3 5 7)))

(defun to-letter (n)
  (cond ((equal n *opponent*) "O")
        ((equal n *computer*) "X")
        (t " ")))

(defun print-row (board)
  (format t "~& ~a | ~a | ~a"
          (to-letter (nth 1 board))
          (to-letter (nth 2 board))
          (to-letter (nth 3 board))))

(defun print-row-separator ()
  (format t "~&---+---+----"))

(defun print-board (board)
  (format t "~&")
  (print-row (nthcdr 0 board))
  (print-row-separator)
  (print-row (nthcdr 3 board))
  (print-row-separator)
  (print-row (nthcdr 6 board)))

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet) (sum-triplet board triplet))
          *triplets*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
        (member (* 3 *opponent*) sums))))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
      pos
      (pick-random-empty-position board))))

(defun board-full-p (board)
  (null (member 0 board)))

(defun find-empty-position (board triplet)
  (find-if #'(lambda (pos) (zerop (nth pos board))) triplet))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board) "random move"))

(defun block-opponent-win (board)
  (let ((pos (win-or-block board (* 2 *opponent*))))
    (and pos (list pos "block opponent"))))

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board (* 2 *computer*))))
    (and pos (list pos "make three in a row"))))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if #'(lambda (trip)
                              (equal (sum-triplet board trip) target-sum))
                          *triplets*)))
    (when triplet (find-empty-position board triplet))))

(defun extract-diagonal (board diag)
  (mapcar )
             ;; TODO(mihai): fix this

(defun block-squeeze-play (board)
  (let ((pattern (list *opponent* *computer* *opponent*)))
    (when (member pattern
             (mapcar "#(lambda (diag) (extract-diagonal board diag))
             ;; TODO(mihai): fix this

(defun choose-best-move (board)
  (or (block-squeeze-play board)
      (make-three-in-a-row board)
      (block-opponent-win board)
      (random-move-strategy board)))

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos) (<= 1 pos 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move board))
          ((not (zerop (nth pos board)))
           (format t "~&That position is already occupied.")
           (read-a-legal-move board))
          (t pos))))

(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move *opponent* pos board)))
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&You win!"))
          ((board-full-p new-board) (format t "~&Tie game."))
          (t (computer-move new-board)))))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&I win!"))
          ((board-full-p new-board) (format t "~&Tie game."))
          (t (opponent-move new-board)))))

(defun play-one-game ()
  (if (y-or-n-p "Would you like to go first? ")
    (opponent-move (make-board))
    (computer-move (make-board))))

;; tests
(defparameter b (make-board))

(print b)
(print-board b)

(print-board (make-move *opponent* 3 b))
(print-board (make-move *computer* 9 b))

(sum-triplet b '(3 6 9))

(compute-sums b)

(print-board (make-move *computer* 7 b))
(print-board (make-move *computer* 8 b))
(winner-p b)

(board-full-p b)

(read-a-legal-move b)

(pick-random-empty-position b)

(computer-move b)

;; run game
(play-one-game)
