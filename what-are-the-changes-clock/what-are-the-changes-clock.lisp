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

(defun pred-perfect-match (val candidate tfs)
  (declare (ignore tfs))
  (string= val candidate))

(defun pred-later-than (val candidate tfs)
  (< (position val tfs :test #'string=)
     (position candidate tfs :test #'string=)))

(defun n-equal (n candidate tfs)
  (declare (ignore tfs))
  (let* ((digits (coerce (remove #\: candidate) 'list))
         (freqs (mapcar #'(lambda (x) (count x digits)) digits)))
    (some (lambda (x) (>= x n)) freqs)))

(defun pred-four-equal (candidate tfs)
  (n-equal 4 candidate tfs))

(defun pred-three-equal (candidate tfs)
  (n-equal 3 candidate tfs))

(defun pred-two-equal (candidate tfs)
  (n-equal 2 candidate tfs))

(defun pred-palindom (candidate tfs)
  (declare (ignore tfs))
  (string= candidate (reverse candidate)))

(defun pred-repeating (candidate tfs)
  (declare (ignore tfs))
  (let* ((colon-pos (position #\: candidate))
         (hour (subseq candidate 0 colon-pos))
         (minute (subseq candidate (1+ colon-pos))))
    (string= hour minute)))

(defun pred-double-on-hand (candidate tfs)
  (declare (ignore tfs))
  (and (equal (aref candidate 0) (aref candidate 1))
       (equal (aref candidate 3) (aref candidate 4))))

(defun pred-sharp (candidate tfs)
  (declare (ignore tfs))
  (string= "00" (subseq candidate 3)))

;; main processor

(defun print-times (times)
  (dotimes (i (length times))
    (when (zerop (mod i 16)) (format t "~&"))
    (format t "~a " (nth i times))))

(defun what-are-the-chances (predicate format)
  (let* ((fun (first predicate))
         (fun-args (rest predicate))
         (tfs (cdr (assoc format time-formats)))
         (items (remove-if-not
                  (lambda (x) (apply fun (append fun-args (list x tfs))))
                  tfs)))
    (print-times items)
    (format t "~&count ~d" (length items))
    (format t "~&percentage: ~,2f%" (* (/ (length items) (length tfs)) 100))))

;; tests

(dolist (tf time-formats)
  (let ((fmt (car tf)))
    (format t "~&~%=======================")
    (format t "~&Checking time format ~a" fmt)
    (what-are-the-chances '(pred-later-than "11:56") fmt)
    (what-are-the-chances '(pred-perfect-match "01:59") fmt)
    (what-are-the-chances '(pred-four-equal) fmt)
    (what-are-the-chances '(pred-three-equal) fmt)
    (what-are-the-chances '(pred-two-equal) fmt)
    (what-are-the-chances '(pred-palindom) fmt)
    (what-are-the-chances '(pred-repeating) fmt)
    (what-are-the-chances '(pred-double-on-hand) fmt)
    (what-are-the-chances '(pred-sharp) fmt)))

;; TODO (mihai): fix this)
;; draw the probability histogram
;; |
;; |    Situation      : ------------------------------------------------------- (100%)
;; |    Later Than     : ==========================
;; |    Perfect Match  : =====================
;; |    Four Equal     : =====================
;; |    Three Equal    : =========================
;; |    Two Equal      : ====================
;; |    Palindom       : ==============
;; |    Repeating      : ==========================
;; |    Double On Hand : ======
;; |    Sharp          : ==============
;; |
