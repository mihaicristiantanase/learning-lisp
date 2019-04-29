(reduce #'+ '(0 1 2 3 4 5 6 7 8 9))
(reduce #'* '(1 2 3 4))

;; 7.16
(reduce #'union '((a b c) (c d a) (f b d) (g)))

;; 7.17
(defun total-length (l)
  (reduce #'+ (mapcar #'length l)))

(defun total-length2 (l)
  (length (reduce #'append l)))

(total-length '((a b c) (c d a) (f b d) (g)))
(total-length2 '((a b c) (c d a) (f b d) (g)))

;; 7.18
(reduce #'+ nil)
(reduce #'* nil)

;; 7.19
(defun all-odd (l)
  (every #'oddp l))
(all-odd '(1 3 7))

;; 7.20
(defun none-odd (l)
  (every #'evenp l))
(none-odd '(2 4 6))

;; 7.21 - better named "find-even"
(defun none-all-odd (l)
  (not (every #'oddp l)))
(none-all-odd '(3 5 7))

;; 7.22 - better named "find-odd"
(defun not-none-odd (l)
  (not (every #'evenp l)))
(not-none-odd '(2 4 6))

;; 7.26 - "find-if" given "remove-if-not"
(car (remove-if-not #'oddp '(2 3 4)))
(defun my-find-if (pred l)
  (car (remove-if-not pred l)))
(my-find-if #'oddp '(2 4 7))

;; 7.27 - "every" given "remove-if"
(defparameter l '(2 8 4 6))
(every #'evenp l)
(equal l (remove-if #'(lambda (x) (not (evenp x))) l))
(null (remove-if #'evenp l))

;; 7.28
EVERY applicative operator graph:
########    ########    ########    ########    ########
#      #    #      #    #      #    #      #    #      #
#      #    #      #    #      #    #      #    #      #
#      #    #      #    #      #    #      #    #      #
########    ########    ########    ########    ########
   |           |           |           |           |
   |           |           |           |           |
   +-----------+-----------+-----------+-----------+---> result

; The book suggests:
########    ########    ########    ########    ########
#      #    #      #    #      #    #      #    #      #
#      #    #      #    #      #    #      #    #      #
#      #    #      #    #      #    #      #    #      #
########    ########    ########    ########    ########
   ?           ?           ?           ?           ?  -----> boolean result

(defun half (n) (* n 0.5))
(defun average (x y) (+ (half x) (half y)))

(defun add-to-end (x l)
  (append l (list x)))

(defun repeat-first (phrase)
  (add-to-end (first phrase) phrase))

(add-to-end 'mihai '(flory is here))
(repeat-first '(mihai an flory))
