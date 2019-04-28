;;; Cards game

(defvar *my-hand* 
  '((3 hearts)
    (5 clubs)
    (2 diamonds)
    (4 diamonds)
    (ace spades)))

(defvar *colors*
  '((clubs black)
    (diamonds red)
    (hearts red)
    (spades black)))

(defvar *all-ranks* '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun rank (card)
  (first card))

(defun suit (card)
  (second card))

(defun count-suit (suit hand)
  (length (remove-if-not
            #'(lambda (card) (equal suit (suit card)))
            hand)))

(defun color-of (card)
  (second (assoc (suit card) *colors*)))

(defun first-red (hand)
  (find-if #'(lambda (card) (equal 'red (color-of card)))
           hand))

(defun black-cards (hand)
  (remove-if-not #'(lambda (card) (equal 'black (color-of card)))
           hand))

(defun what-ranks (suit hand)
  (mapcar #'first
          (remove-if-not #'(lambda (card) (equal suit (suit card))) hand)))

(defun beforep (x y l)
  (> (length (member x l)) (length (member y l))))

(defun higher-rank-p (card1 card2)
  (beforep (rank card2) (rank card1) *all-ranks*))

(defun first-of-rank (rank hand)
  (find-if #'(lambda (card) (equal rank (rank card))) hand))

(defun high-card (hand)
  ; TODO(mihai): the same value is used, how to fix this?
  (cond ((first-of-rank 'ace hand) (first-of-rank 'ace hand))
        (t nil)))

;; Tests

(rank '(2 spades))
(suit '(2 spades))
(count-suit 'diamonds *my-hand*)
(color-of '(3 hearts))

(first-red '((3 clubs)
             (5 clubs)
             (ace spades)))

(black-cards '((3 diamonds)
               (5 clubs)
               (ace diamonds)))

(what-ranks 'clubs *my-hand*)

(higher-rank-p '(9 diamonds) '(ace clubs))

(first-of-rank '2 *my-hand*)

(high-card *my-hand*)
