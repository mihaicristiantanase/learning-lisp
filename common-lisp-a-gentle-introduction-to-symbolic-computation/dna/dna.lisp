;;; Single or Double Stranded DNA (deoxyribonucleic acid)

;; A strand of DNA is a chain of 4 base types:
;; A: Adenine
;; T: Thymine
;; G: Guanine
;; C: Cytosine
;; Each of the four bases have has a complement with which it can form a pair:
;; A ↔ T
;; G ↔ C
;; Two strands of DNA can be combined to form the Double Stranded DNA (the
;; "double helix" form)

(defun complement-base (base)
  (second (assoc base '((A T) (T A) (G C) (C G)))))

(defun complement-strand (strand)
  (mapcar #'complement-base strand))

(defun make-double (strand)
  (mapcar #'(lambda (base) (list base (complement-base base))) strand))

;; TODO(mihai): why is calling this function multiple times does not reset the
;; "table" value?
(defun count-bases (dna)
  (let ((table '((A 0) (T 0) (G 0) (C 0))))
    (dolist (elem dna table)
      (if (listp elem)
        (progn
          (incf (second (assoc (first elem) table)))
          (incf (second (assoc (second elem) table))))
        (incf (second (assoc elem table)))))))

(defun prefixp (strand1 strand2)
  (do ((s1 strand1 (rest s1))
       (s2 strand2 (rest s2)))
    ((null s1) t)
    (unless (equal (first s1) (first s2)) (return nil))))

(defun appearsp (strand1 strand2)
  (do ((s2 strand2 (rest s2)))
    ((null s2) nil)
    (when (prefixp strand1 s2) (return t))))

(defun coverp (strand1 strand2)
  (do ((l1 (length strand1))
       (s2 strand2 (nthcdr l1 s2)))
    ((null s2) t)
    (unless (prefixp strand1 s2) (return nil))))

(defun prefix (n strand)
  (let ((p '()))
    (dotimes (i n p)
      (setf p (append p (list (nth i strand)))))))

(defun kernel (strand)
  (dotimes (l (length strand))
    (let ((candidate (prefix (+ 1 l) strand)))
      (when (coverp candidate strand) (return candidate)))))

(defun draw-dna (strand)
  (labels ((draw-dna-line (n f)
                          (format t "~&")
                          (dotimes (i n) (format t (funcall f i))))
           (format-base (base)
                        (format nil "  ~a   " base)))
    (let ((l (length strand))
          (complement (complement-strand strand)))
      (draw-dna-line l #'(lambda (i) "------"))
      (draw-dna-line l #'(lambda (i) "  !   "))
      (draw-dna-line l #'(lambda (i) (format-base (nth i strand))))
      (draw-dna-line l #'(lambda (i) "  ·   "))
      (draw-dna-line l #'(lambda (i) "  ·   "))
      (draw-dna-line l #'(lambda (i) (format-base (nth i complement))))
      (draw-dna-line l #'(lambda (i) "  !   "))
      (draw-dna-line l #'(lambda (i) "------"))
      (format t "~&"))))

(defparameter bases '(A T G C))

(defun generate-dna (n)
    (do ((lb (length bases))
         (dna '())
         (i 0 (1+ i)))
      ((>= i n) dna)
      (push (nth (random lb) bases) dna)))

(defun test-kernel ()
  (do* ((l-dna (+ 1 (random 3000)))
        (dna (generate-dna l-dna))
        (k (kernel dna))
        (l-k (length k))
        (i 0 (1+ i)))
    ((>= i 1000000) 'done)
    (when (< l-k l-dna)
      (format t "~&DNA of length ~a has kernel of length ~a" l-dna l-k))))

;; Tests

(complement-base 'A)
(complement-base 'T)
(complement-base 'G)
(complement-base 'C)

(complement-strand '(A G G T))

(make-double '(A G G T))

(count-bases '(A G G T))

(count-bases '(A G T A C T C T))

(count-bases '((T A) (C G) (A T) (T A) (G C) (A T) (G C) (A T)))

(prefixp '(G C T) '(G C T A T))
(prefixp '(G C T) '(C C T A T))

(appearsp '(G C T) '(A T C G C T))

(nthcdr 1 '(a b c d))

(coverp '(C A T) '(C A T C A T C A T C A T))
(coverp '(C A T) '(C A T C A T C A T C A))

(prefix 4 '(C G A T T A G))

(kernel '(A G C A G C A G C))
(kernel '(A A A A A A A A A A))
(kernel '(A G G T C))

(draw-dna '(A T G C))
(draw-dna '(A G C A G C A G C))

(draw-dna '(A G C A G C A G C C A G C A G C))

(time (draw-dna '(A G C A G C A G C C A G C A G C)))

;; Time
(generate-dna 30)
(test-kernel)

(time (test-kernel))
