;;;; randomizing texts

(defun is-separator (c)
  (or (eq c #\Space) (eq c #\,)))

(defun split (text)
  (let ((chars (loop for c across text collect c))
        (words '())
        (current-word '()))
    (dolist (c chars)
      (if (is-separator c)
        (progn
          (push (coerce (reverse current-word) 'string) words)
          (setf current-word '()))
        (push c current-word)))
    (when (> (length current-word) 0)
      (push (coerce (reverse current-word) 'string) words))
    (reverse (remove "" words :test #'string=))))

(split "mihai is here   hehe")

(defun join (lst)
  (format nil "~{~a~^ ~}" lst))

(join '(1 2 9 54))

(defun shuffle-with-indexes (l indexes)
  (let ((result '()))
    (dolist (idx indexes)
      (push (nth idx l) result))
    result))

(defvar rand-state (make-random-state))

(defun shuffle (l)
  (let ((indexes (loop for i below (length l) collect i))
        (shuffle-indexes '()))
    (loop (let ((idx (random (length indexes) rand-state)))
            (let ((item (nth idx indexes)))
              (push item shuffle-indexes)
              (setf indexes (remove item indexes)))
            (when (null indexes)
              (return (shuffle-with-indexes l shuffle-indexes)))))))

(shuffle '(a b c d e))

(shuffle-with-indexes '(a b c d e) '(0 1 4 3 2))

(split "
        Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy
        eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam
        voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita
        kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.
       ")

(defun rand (text)
  (let ((words (split text)))
    (join (shuffle words))))

(if (null (cdr *posix-argv*))
  (format t "Should pass the text to randomize~%")
  (let ((res (rand (cadr *posix-argv*))))
    (format t "~a~%" res)))
