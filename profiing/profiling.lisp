;; Processes a CSV file and displays the duration of each action step
;; The format of an action (line) is: TIMESTAMP (SECONDS),ACTION,COMMENTS

(ql:quickload "parse-float")

(defun parse-line (line)
  (let ((tokens (split-sequence:split-sequence #\, line)))
    (list (parse-float:parse-float (first tokens) :type 'double-float :junk-allowed t)
          (second tokens))))

(defun parse (csv)
  (mapcar #'parse-line csv))

(defun show-durations-rec (parsed-csv)
  (when (> (length parsed-csv) 1)
    (let ((a1 (first parsed-csv))
          (a2 (second parsed-csv)))
      (format t "~&Duration of ~a:~49t ~5,3F"
              (second a1)
              (- (first a2) (first a1))))
    (show-durations-rec (rest parsed-csv))))

(defun show-durations (csv)
  (show-durations-rec (parse csv)))

(defun get-file-contents (fname)
  (with-open-file (s fname)
    (do ((lines '())
         (l (read-line s) (read-line s nil 'eof)))
        ((eq l 'eof) lines)
      (setf lines (append lines (list l))))))

(defparameter fname "~/workspace/orange-money-ios/res/OneSpan/PinTesting/res/registration-profiling3.csv")

(show-durations (rest (get-file-contents fname)))
