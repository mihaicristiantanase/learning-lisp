;; Decipher the cryptogram

(print "And we begin")

(defvar *crypto-text*
  '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv"
    "jupi jf enlpo pib slafml pvv bfwkj"))

(defun decipher (encrypted decipher-table)
  (if (null decipher-table)
      encrypted
      (let ((tuple (first decipher-table)))
            (decipher (substitute (car tuple) (cdr tuple) encrypted)
                      (rest decipher-table)))))

(defun display-status (encrypted decipher-table)
  (format t "~&----------------------------------------")
  (dolist (s encrypted)
    (format t "~&~a" s)
    (format t "~&~a" (decipher s decipher-table)))
  (format t "~&----------------------------------------"))

(substitute  #\a #\b "ababbfb")

(defun solve-locally (encrypted encipher-table decipher-table)
  (let ((decipher-table '())
        (encipher-table '()))
    (display-status encrypted decipher-table)
    (format t "~&Substitute which letter?")))

(defun solve (encrypted)
  (let ((encipher-table '())
        (decipher-table '()))
    (solve-locally encrypted encipher-table decipher-table)))

(solve *crypto-text*)
