;;; Computes the chances that you see a digital clock combination

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

;; predicates

(defun perfect-match (val candidate tfs)
  (declare (ignore tfs))
  (string= val candidate))

(defun later-than (val candidate tfs)
  (< (position val tfs :test #'string=)
     (position candidate tfs :test #'string=)))

(defun n-equal (n candidate tfs)
  (declare (ignore tfs))
  (let* ((digits (coerce (remove #\: candidate) 'list))
         (freqs (mapcar #'(lambda (x) (count x digits)) digits)))
    (some (lambda (x) (>= x n)) freqs)))

(defun four-equal (candidate tfs)
  (n-equal 4 candidate tfs))

(defun three-equal (candidate tfs)
  (n-equal 3 candidate tfs))

(defun two-equal (candidate tfs)
  (n-equal 2 candidate tfs))

;; main processor

(defun what-are-the-chances (predicate format)
  (let* ((fun (first predicate))
         (fun-args (rest predicate))
         (tfs (cdr (assoc format time-formats)))
         (items (remove-if-not
                  (lambda (x) (apply fun (append fun-args (list x tfs))))
                  tfs)))
    (format t "items:~&~{~a ~}" items)
    (length items)))

;; tests

(what-are-the-chances '(perfect-match "01:59") 'fmt12)
(what-are-the-chances '(later-than "23:58") 'fmt24)
(what-are-the-chances '(four-equal) 'fmt24)
(what-are-the-chances '(three-equal) 'fmt24)
(what-are-the-chances '(two-equal) 'fmt24)
(what-are-the-chances '(four-equal) 'fmt12)
(what-are-the-chances '(three-equal) 'fmt12)
(what-are-the-chances '(two-equal) 'fmt12)
