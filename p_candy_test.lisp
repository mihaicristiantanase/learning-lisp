; reads "input.txt" and "output.txt"
; prints all the lines that give the wrong result

(load "p_candy.lisp")

(defun split-by-one-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (if string
        (loop for i = 0 then (1+ j)
              as j = (position #\Space string :start i)
              collect (subseq string i j)
              while j)
        '()
    )
)

(defun make-list-int (L)
    (if L
        (cons (parse-integer (car L)) (make-list-int (cdr L)))
        '()
    )
)

(defun process-files (in out line-no)
    (let ((in-line (make-list-int (split-by-one-space (read-line in nil nil))))
          (out-line (make-list-int (split-by-one-space (read-line out nil nil)))))
        (if (and in-line out-line)
            (let ((expected (give-candy in-line)))
                ;(format t "Processing ~S and ~S, expecting ~S" 
                ;        in-line out-line expected)
                (if (equal out-line expected)
                    (process-files in out (+ line-no 1))
                    (format t "Error at line ~S:~% result ~S~% expect ~S~%" 
                            line-no out-line expected)
                )
            )
            (if (or in-line out-line)
                (format t "File sizes do not match~%")
                (format t "Everything is ok!~%")
            )
        )
    )
)

(defun execute-tests (input output)
    (format t "Processing files ~S and ~S~%"
            input output)
    (let ((in (open input))
          (out (open output)))
        (if (and in out)
            (process-files in out 1)
            (format t "Either ~S or ~S are missing."
                    input output)
        )
    )
)

(defun gen-rand (n)
    (if (<= n 0)
        '()
        (cons (+ 1 (random 100)) (gen-rand (- n 1)))
    )
)

(defun numlist-to-string (lst)
    (when lst
        (concatenate 'string " "
        (write-to-string (car lst)) (numlist-to-string (cdr lst))))
)

(defun gen-rand-children-rec (output-stream-in output-stream-out n)
    (if (<= n 0)
        (progn
            (close output-stream-in)
            (close output-stream-out)
        )
        (let ((rand_list (gen-rand 200)))
            ;(format output-stream-in "~S~%"
            ;        rand_list)
            ;(format output-stream-out "~S~%"
            ;        (give-candy rand_list))
            (write-line (numlist-to-string rand_list) output-stream-in)
            (write-line (numlist-to-string (give-candy rand_list)) output-stream-out)
            (gen-rand-children-rec output-stream-in output-stream-out (- n 1))
        )
    )
)

(defun gen-rand-children (output_in output_out n)
    (let (
          (stream-in (open output_in :direction :output :if-exists :supersede))
          (stream-out (open output_out :direction :output :if-exists :supersede))
         )
        (if (and stream-in stream-out)
            (gen-rand-children-rec stream-in stream-out n)
            (format t "Unable to open ~S or ~S~%" output_in output_out)
        )
    )
)
