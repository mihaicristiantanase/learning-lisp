;;; Calculates the user ratings required to reach a given target

(defun always-valid (value)
  (declare (ignore value))
  t)

(defun prompt (msg &optional (validator-p #'always-valid) (validator-msg ""))
  (format t msg)
  (finish-output)
  (let ((value (read)))
    (if (funcall validator-p value)
      value
      (progn
        (when (not (equal "" validator-msg)) (format t "~&~a" validator-msg))
        (prompt msg validator-p validator-msg)))))

(defun compute-required-users (current-rating rating-count target-rating expected-rating)
  (let ((diff (- expected-rating target-rating)))
    (if (zerop diff)
      "Inf"
      (ceiling
        (/ (* (- target-rating current-rating) rating-count)
           diff)))))

(let ((current-rating
        (prompt "~&Current rating  : "
                #'(lambda (r) (and (numberp r) (<= 1 r 5)))
                "Rating should be a value between 1 and 5"))
      (rating-count
        (prompt "Rating count    : "
                #'(lambda (n) (and (numberp n) (>= n 1)))
                "The rating count should be at least 1"))
      (target-rating
        (prompt "Target rating   : "
                #'(lambda (r) (and (numberp r) (<= 1 r 5)))
                "Rating should be a value between 1 and 5"))
      (expected-rating
        (prompt "Expected rating : "
                #'(lambda (r) (and (numberp r) (<= 1 r 5)))
                "Rating should be a value between 1 and 5")))
  (format t "------------------------------")
  (format t "~&Number of users required : ~a"
          (compute-required-users
            current-rating
            rating-count
            target-rating
            expected-rating)))
