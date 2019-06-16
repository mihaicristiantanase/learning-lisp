;; 10.3

(defparameter *friends* nil)
(defparameter *freq-friends* nil)

(defun meet (person)
  (cond ((equal person (first *friends*))
         'we-just-met
         (incf (cdr (assoc person *freq-friends*))))
        ((member person *friends*)
         'we-know-each-other
         (incf (cdr (assoc person *freq-friends*))))
        (t (push person *friends*)
           (push (cons person 1) *freq-friends*)
           'please-to-meet-you)))

(defun best-friends ()
  (remove-if #'(lambda (x)
                 (<= (cdr (assoc x *freq-friends*)) 1))
             *friends*))

;; 10.4
(defun forget (person)
  (if (member person *friends*)
    (progn
      (setf *friends* (remove person *friends*))
      (setf *freq-friends*
            (remove-if #'(lambda (x) (equal person (car x)))
                       *freq-friends*)))
    (format t "~a is not my friend~%" person)))

(meet 'fred)
(meet 'joe)
(meet 'monika)

(meet 'mihai)

(best-friends)
*friends*
*freq-friends*

(forget 'cucu)
(forget 'monika)

;; 10.5
(defun not-so-ugly (x y)
  (let* ((m (max x y))
         (avg (/ (+ x y) 2.0))
         (pct (* 100 (/ avg m))))
    (list 'average avg 'is pct 'percent 'of 'max m)))

(not-so-ugly 10 20)

;; 10.6
(defparameter x nil)
(push x x)
(push x x)
(push x x)

;; 10.7
(defparameter x '(1 2 3))
(setf (length x) 99) ; does not work because "length" does not specify a valid place description
(setf (nth 2 x) 99) ; works
x
