;;; Play craps

(defun throw-die ()
  "Returns a random number between 1 and 6 inclusive."
  (+ 1 (random 6)))

(defun throw-dice ()
  "Returns a dice 'throw' as a 2 element list."
  (list (throw-die) (throw-die)))

(defun snake-eyes-p (throw)
  "Returns true if the throw is a (1 1)."
  (equal '(1 1) throw))

(defun boxcars-p (throw)
  "Returns true if the throw is a (6 6)."
  (equal '(6 6) throw))

(defun throw-sum (throw)
  "Returns the sum of a dice throw"
  (+ (first throw) (second throw)))

(defun instant-win-p (throw)
  "Returns true if the throw is 7 or 11."
  (member (throw-sum throw) '(7 11)))

(defun instant-loss-p (throw)
  "Returns true if the throw is 2, 3 or 12."
  (member (throw-sum throw) '(2 3 12)))

(defun say-throw (throw)
  "Returns the sum of the throw or snake-eyes or boxcars when sum is either 2
  or 12."
  (let ((sum (throw-sum throw)))
    (cond ((equal 2 sum) 'snakeeyes)
          ((equal 12 sum) 'boxcars)
          (t sum))))

(defun craps ()
  "Returns a play step."
  (let* ((throw (throw-dice))
         (say (say-throw throw)))
    (append (list
              'THROW
              (first throw)
              'AND
              (second throw)
              '--
              say
              '--)
            (cond ((instant-win-p throw) '(YOU WIN))
                  ((instant-loss-p throw) '(YOU LOSE))
                  (t (list 'YOUR 'POINT 'IS say))))))

(defun try-for-point (point)
  "Returns a try for a point."
  (let* ((throw (throw-dice))
         (sum (throw-sum throw)))
    (append (list 'THROW (first throw) 'AND (second throw) '-- sum '--)
            (cond ((equal point sum) '(YOU WIN))
                  ((equal 7 sum) '(YOU LOSE))
                  (t '(THROW AGAIN))))))

(throw-die)
(throw-dice)
(snake-eyes-p '(1 1))
(boxcars-p '(6 6))
(snake-eyes-p '(2 1))
(boxcars-p '(5 5))
(instant-win-p '(1 5))
(instant-loss-p '(1 3))
(say-throw '(1 1))
(craps)
(try-for-point 9)

(documentation 'throw-die 'function)

(documentation 'throw-dice 'function)

(documentation 'snake-eyes-p 'function)

(documentation 'boxcars-p 'function)

(documentation 'instant-win-p 'function)

(documentation 'instant-loss-p 'function)

(documentation 'say-throw 'function)

(documentation 'craps 'function)

(documentation 'try-for-point 'function)
