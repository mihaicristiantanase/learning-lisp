;; Computes the chances that you see a digital clock combination

(defun add-entrire-hour (h)
  (let ((lst '()))
    (dotimes (m 60)
      (push (format nil "~2,'0d:~2,'0d" h m) lst))
    (reverse lst)))

(defun gen-time-table (tf)
  (let* ((str (write-to-string tf))
         (nr-hours (parse-integer (subseq str 3)))
         (lst '()))
    (dotimes (h nr-hours)
      (setf lst (append lst (add-entrire-hour h))))
    (when (equal 'fmt12 tf)
      (setf lst (append lst (add-entrire-hour 12)))
      (do ((h 1 (1+ h)))
          ((> h 11))
          (setf lst (append lst (add-entrire-hour h)))))
    lst))

(defun cell-gen-table (format)
  (cons format (gen-time-table format)))

(defparameter time-formats
  (list
    (cell-gen-table 'fmt12)
    (cell-gen-table 'fmt24)))

(defun what-are-the-chances (time format)
  (let* ((pos (position #\: time))
         (hour (subseq time 0 pos))
         (minute (subseq time (+ pos 1))))
    (count time (cdr (assoc format time-formats)) :test 'string=)))

(what-are-the-chances "00:12" 'fmt12)
