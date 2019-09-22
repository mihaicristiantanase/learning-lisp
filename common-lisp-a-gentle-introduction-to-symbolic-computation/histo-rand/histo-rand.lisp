;; Histogram of random numbers
;;
;; This tests how good the random number generator in Lisp is
;;

(defvar *hist-array* #())
(defvar *total-points* 0)

(defun new-historygram (bins)
  (setf *hist-array* (make-array bins :initial-element 0))
  (setf *total-points* 0))

(defun record-value (value)
  (incf (aref *hist-array* value))
  (incf *total-points*))

(defun print-hist-line (idx)
  (let ((value (aref *hist-array* idx)))
    (format t "~&~2d [~3d] " idx value)
    (dotimes (i value) (format t "*"))))

(defun print-histogram ()
  (dotimes (idx (length *hist-array*))
    (print-hist-line idx))
  (format t "~&   [~3d] total~%" *total-points*))

(defun test-randomness (bins &key (repeat-count 10))
  (new-historygram bins)
  (dotimes (i repeat-count)
    (record-value (random bins)))
  (print-histogram))

; tests

(new-historygram 11)
(record-value 4)
(print-hist-line 4)
(print-histogram)

(test-randomness 30 :repeat-count 2000)
