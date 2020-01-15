;; Decipher the cryptogram
(declaim (optimize debug))

(print "And we begin")

(defvar *crypto-text*
  '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv"
    "jupi jf enlpo pib slafml pvv bfwkj"))

(defun decipher (encrypted decipher-table)
  (dotimes (i (length encrypted))
    (let* ((e-character (aref encrypted i))
           (d-character (cdr (assoc e-character decipher-table)))
           (printed-character (or d-character #\Space)))
      (format t "~a" printed-character))))

(defun display-status (encrypted decipher-table)
  (format t "~&---------------------------------------------------")
  (dolist (s encrypted)
    (format t "~&~a" s)
    (format t "~&")
    (decipher s decipher-table))
  (format t "~&---------------------------------------------------")
  (format t "~%"))

(substitute  #\a #\b "ababbfb")

(defun read-input (message)
  (finish-output)
  (format t "~&~a " message)
  (labels ((read-until-something-useful ()
             (finish-output)
             (let ((line (read-line)))
               (if (zerop (length line))
                   (read-until-something-useful)
                   line))))
    (read-until-something-useful)))

(defun read-character (message)
  (finish-output)
  (format t "~&~a " message)
  (finish-output)
  (read-char))

(defun solve-locally (encrypted decipher-table)
  (display-status encrypted decipher-table)
  (let ((source-input (read-input "Substitute which letter?")))
    (cond ((equal source-input "undo")
           (solve-locally encrypted (remove (assoc
                                              (read-character "Undo which character?")
                                              decipher-table) decipher-table)))
          ((equal source-input "quit")
           (format t "~&Thank you for playing!"))
          (t (let* ((source-character (aref source-input 0))
                    (dest-character (read-character (format nil "What does '~a' decipher to?" source-character))))
               (let ((e-existing (car (assoc source-character decipher-table)))
                     (d-existing (car (rassoc dest-character decipher-table))))
                 (cond (e-existing
                        (format t "~&But '~a' already deciphers to '~a'!" source-character e-existing))
                       (d-existing
                        (format t "~&But '~a' already deciphers to '~a'!" d-existing dest-character))
                       (t (push (cons source-character dest-character) decipher-table)))
                 (solve-locally encrypted decipher-table)))))))

(defun show-frequencies (text)
  (let ((dict '()))
    (dotimes (i (length text))
      (let ((character (aref text i)))
        (when (null (cdr (assoc character dict)))
          (push (cons character 0) dict))
        (incf (cdr (assoc character dict)))))
    (format t "~&The frequencies: ")
    (let ((sorted (sort dict #'(lambda (x y) (> (cdr x) (cdr y))))))
      (dolist (item sorted) (format t "~&~s" item)))))

(defun solve (encrypted)
  (show-frequencies (apply #'concatenate 'string encrypted))
  (let ((decipher-table '()))
    (solve-locally encrypted decipher-table)))

(solve *crypto-text*)
