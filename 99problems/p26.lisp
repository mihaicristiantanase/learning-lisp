; P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
; In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.
;
; Example:
; * (combination 3 '(a b c d e f))
; ((A B C) (A B D) (A B E) ... )
;
; D E F G E F G F G G E F
; C C C C D D D E E F D D
; B B B B B B B B B B C C 
; A A A A A A A A A A A A
;
; 4 5 6 7 5 6 7 6 7 7 5 6 ... 7 * * 5 
; 3 3 3 3 4 4 4 5 5 6 4 4 ... 6 7 * 4
; 2 2 2 2 2 2 2 2 2 2 3 3 ... 5 6 7 3 
; 1 1 1 1 1 1 1 1 1 1 1 1 ... 1 1 1 2
;
; pop until stack is empty or a value greater is found
;

(load "p03.lisp")
(load "p17.lisp")

;
; next-combination
; input -> output
;       ->   4
;       ->   3
;   2   ->   2
;   1   ->   1
(defun build-up (L max_value target_size)
    ;(format t "build-up ~S with max ~S and target ~S~%"
    ;        L max_value target_size)
    (if (>= (length L) target_size)
        ; it's sufficient
        (let ((splitted (split L target_size)))
            (if (listp (car splitted))
                (car (cdr splitted))
                L
            )
        )
        ; needs more
        (if (>= (car L) max_value)
            '()
            (build-up (cons (+ 1 (car L)) L) max_value target_size)
        )
    )
)

;
; next-combination
; input -> output
;   4   ->   5
;   3   ->   3
;   2   ->   2
;   1   ->   1
;
; or:
;   3   ->   3
;   1   ->   2
(defun next-combination (L max_value target_size skip)
    ;(format t "next-combination ~S with max ~S and target ~S~%"
    ;        L max_value target_size)
    (if L
        (let ((build-up-res (build-up L max_value target_size)))
            (if (and build-up-res (not skip))
                ; the build-up was succesful
                build-up-res
                ; the build-up failed
                (let ((next-head (+ 1 (car L))))
                    ;(format t "next-head ~S~%"
                    ;        next-head)
                    (if (> next-head max_value)
                        ; pop and...
                        (if (cdr L)
                            ; ...continue
                            (next-combination (cons (+ 1 (car (cdr L))) (cdr (cdr L))) max_value target_size nil)
                            ; ...stop
                            '()
                        )
                        ; build-up
                        (let ((build-up-res (build-up (cons next-head (cdr L)) max_value target_size)))
                            (if build-up-res
                                ; the build-up was succesful
                                build-up-res
                                ; the build-up failed
                                (next-combination (cons next-head (cdr L)) max_value target_size nil)
                            )
                        )
                    )
                )
            )
        )
        '()
    )
)

(defun gen-comb (L max_value target_size)
    (let ((comb (next-combination L max_value target_size nil)))
        (if comb
            (cons (reverse comb) (gen-comb (next-combination comb max_value target_size 'Y) max_value target_size))
            '()
        )
    )
)

(defun combination-idx (k L)
    (gen-comb '(1) (length L) k)
)

(defun value-from-idx-l (L L_values)
    (if L
        (cons (element-at L_values (car L)) (value-from-idx-l (cdr L) L_values))
        '()
    )
)

(defun value-from-idx-ll (L L_values)
    (if L
        (cons (value-from-idx-l (car L) L_values) (value-from-idx-ll (cdr L) L_values))
        '()
    )
)

(defun combination (k L)
    (value-from-idx-ll (combination-idx k L) L)
)
