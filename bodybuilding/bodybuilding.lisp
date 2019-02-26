(defvar *filename* "~/workspace/body_building/double_progression.csv")
(defun load-file (fname)
  (with-open-file (in fname)
    (with-standard-io-syntax
      (read in))))
(load-file *filename*)

(let ((in (open *filename*)))
  (format t "~a~%" (read-line in)))


(defvar *db*
  (list :data "data" :exercitiu "exercitiu" :seturi "seturi" :repetari "repetari" :greutate  "greutate"))
(format t "~%~{~a:~12t~a~%~}" *db*)

(dolist (elem *db*)
  (format t "~a~%" elem))
(length *db*)

(setf (getf *db* :exercitiu) "flotari")
*db*

(defun make-keyword (name) (values (intern name "KEYWORD")))

(defun make-dictionary (keys values)
  (when (= (length keys) (length values))
    (let ((dict '()))
      (dotimes (i (length keys))
        (let ((key (make-keyword (elt keys i)))
              (value (elt values i)))
          (setf (getf dict key) value)))
      dict)))
(make-dictionary '("a" "b" "c") '(1 2 3))

(let ((keys '("data" "exercitiu" "seturi" "repetari" "greutate"))
      (values '(12 "Flotari" 3 5 120)))
  (let ((dict (make-dictionary keys values)))
    (format t "~%~{~a:~12t~a~%~}" dict)))

(values '(1 2 3))
(describe #'values)
(describe #'intern)

(split-sequence "a" "mihai si flory")

(subseq  "mihai si flory" 2 8)
