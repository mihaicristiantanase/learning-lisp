;; 8.4
(defun laugh (n)
  (if (<= n 0) nil
    (cons 'ha (laugh (1- n)))))

(laugh 4)

(load "../../dtrace.lisp")
(dtrace laugh)

(laugh -1)

;; 8.5
(defun add-up (l)
  (if (null l) 0
    (+ (first l) (add-up (rest l)))))

(add-up '(1 2 3 4))

;; 8.6
(defun alloddp (l)
  (cond ((null l) t)
        ((evenp (first l)) nil)
        (t (alloddp (rest l)))))

(alloddp '(1 3 5 9))

;; 8.7
(defun rec-member (x l)
  (cond ((equal x (first l)) t)
        ((null l) nil)
        (t (rec-member x (rest l)))))

(rec-member 'mihia '(flory Mihia 1234))

(rec-member 'mihia '(mihia))

;; 8.8
(defun rec-assoc (x l)
  (cond ((null l) nil)
        ((equal x (first (first l))) (first l))
        (t (rec-assoc x (rest l)))))

(rec-assoc 2 '((5 mihai)
               (3 flory)))

;; 8.9
(defun rec-nth (n l)
  (cond ((null l) nil)
        ((= n 0) (first l))
        (t (rec-nth (1- n) (rest l)))))

(rec-nth 2 '(1 2 3 4 5))

(dtrace rec-nth)

;; 8.10
(defun add1 (x) (1+ x))
(defun sub1 (x) (1- x))

(defun rec-plus (x y)
  (cond ((zerop y) x)
        (t (add1 (rec-plus x (sub1 y))))))

(rec-plus 100 100\0)

;; 8.11

(defun fib (n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (t (+ (fib (- n 1)) (fib (- n 2))))))

(fib 1)
(fib 4)
(fib 5)
(fib 30)

;; 8.14
(defun infinite () (infinite))
(infinite)

;; 8.17 - double tail recursion
(defun find-first-odd (l)
  (cond ((null l) nil)
        ((oddp (first l)) (first l))
        (t (find-first-odd (rest l)))))
(find-first-odd '())

;; 8.18 - single tail recursion
(defun last-element (l)
  (cond ((atom (cdr l)) (car l))
        (t (last-element (rest l)))))

(last-element '(1 (2 3) ((4 5))))

;; 8.21
(defun add-nums (n)
  (cond ((= n 0) 0)
        (t (+ n (add-nums (1- n))))))

(add-nums 5)

;; 8.22
(defun all-equal (l)
  (cond ((null (rest l)) t)
        ((not (equal (first l) (second l))) nil)
        (t (all-equal (rest l)))))

(all-equal '())
(all-equal '(1))
(all-equal '(2 1))
(all-equal '(1 1))
(all-equal '(1 1 1 1))
(all-equal '(1 1 E 1))

;; 8.24
(defun count-down (n)
  (cond ((zerop n) nil)
        (t (cons n (count-down (1- n))))))

(count-down 5)

;; 8.25
(defun fact-applicative (n)
  (reduce #'* (count-down n)))

(fact-applicative 5)

;; 8.26
(defun count-down-to-0 (n)
  (cond ((= n -1) nil)
        (t (cons n (count-down-to-0 (1- n))))))

(defun count-down-to-0-2 (n)
  (cond ((zerop n) '(0) )
        (t (cons n (count-down-to-0-2 (1- n))))))

(count-down-to-0 5)
(count-down-to-0-2 5)

;; 8.27
(defun square-list (l)
  (cond ((null l) nil)
        (t (cons (* (first l) (first l)) (square-list (rest l))))))

(square-list '(3 2 5 9))

;; 8.28
(defun my-nth (n l)
  (cond ((or (zerop n) (null l)) (first l))
        (t (my-nth (1- n) (rest l)))))

(my-nth 1000 '(a b c))
(my-nth 1 '(a b c))
(my-nth 0 '(a b c))

;; 8.29
(defun my-member (x l)
  (cond ((null l) nil)
        ((equal x (first l)) l)
        (t (my-member x (rest l)))))

(my-member 'c '(b a c))

;; 8.30
(defun my-assoc (x l)
  (cond ((null l) nil)
        ((equal x (car (first l))) (first l))
        (t (my-assoc x (rest l)))))

(my-assoc 'x '((x 1) (y 2) (z 3)))

;; 8.31
(defun compare-lengths (l1 l2)
  (cond ((and (null l1) (null l2)) 'same-length)
        ((null l2) 'first-is-longer)
        ((null l1) 'second-is-longer)
        (t (compare-lengths (rest l1) (rest l2)))))

(compare-lengths '(1 2 3) '(a b c d e f g))
(compare-lengths '(1 2 3) '(a b c))
(compare-lengths '(0 1 2 3 4 5 6 7 8 9) '(a b c))

;; 8.32
(defun sum-numeric-elements (l)
  (cond ((null l) 0)
        ((numberp (first l)) (+ (first l) (sum-numeric-elements (rest l))))
        (t (sum-numeric-elements (rest l)))))

(sum-numeric-elements '(3 bears and 3 bowls and 1 girl))

;; 8.33
(defun my-remove (x l)
  (cond ((null l) nil)
        ((equal x (first l)) (my-remove x (rest l)))
        (t (cons (first l) (my-remove x (rest l))))))

(my-remove 'x '(a x c x d e x))`

;; 8.34
(defun my-intersection (x y)
  (cond ((or (null x) (null y)) nil)
        ((member (first y) x) (cons (first y) (my-intersection x (rest y))))
        (t (my-intersection x (rest y)))))

(my-intersection '(a b a c d) '(1 2 a d e))
(my-intersection '(a b) '(1 2 a d e))
(my-intersection '(b) '(1 2 a d e))
(my-intersection '(1 2 a d e) '(a b a))

;; 8.35
(defun my-set-difference (x y)
  (cond ((null x) nil)
        ((member (first x) y) (my-set-difference (rest x) y))
        (t (cons (first x) (my-set-difference (rest x) y)))))

(my-set-difference '(a b c 0) '(a b 3 9))

;; 8.36
(defun count-odd (l)
  (cond ((null l) 0)
        ((oddp (first l)) (+ 1 (count-odd (rest l))))
        (t (count-odd (rest l)))))

(count-odd '(0 1 2 3 4 5 6 7 8 9))

;; 8.37
(defun combine (x y) (+ x y))
(combine 1 3)

(defun fib2 (n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (t (combine (fib2 (- n 1)) (fib2 (- n 2))))))

(fib2 5)

;; 8.38
(defun atoms-to-q (x)
  (cond ((null x) nil)
        ((atom x) 'q)
        (t (cons (atoms-to-q (car x))
                 (atoms-to-q (cdr x))))))

(atoms-to-q '(1 2 (a b (1 mihai 99))))

(atoms-to-q '(1 a (mihai)))

;; 8.39
(defun count-atoms (tree)
  (cond ((atom tree) 1)
        (t (+ (count-atoms (car tree))
              (count-atoms (cdr tree))))))

(count-atoms '(a (b) c))

;; 8.40
(defun count-cons (tree)
  (cond ((atom tree) 0)
        (t (+ 1
              (count-cons (car tree))
              (count-cons (cdr tree))))))

(count-cons 'a)
(count-cons '(a))
(count-cons '(a b))
(count-cons '(a b c))
(count-cons '((a)))
(count-cons '((a)))

;; 8.41
(defun sum-tree (tree)
  (cond ((numberp tree) tree)
        ((atom tree) 0)
        (t (+ (sum-tree (car tree))
              (sum-tree (cdr tree))))))

(sum-tree '())
(sum-tree '(a b c))
(sum-tree '(a b 1 c))
(sum-tree '(a b 1 c 9 10))
(sum-tree '((3 bears) (3 bowls) (1 girl)))

;; 8.42
(defun my-subst (d s l)
  (cond ((equal s l) d)
        ((atom l) l)
        (t (cons (my-subst d s (car l))
                 (my-subst d s (cdr l))))))

(my-subst 'z 'a '((mihai a)))

(my-subst 'z 'a '(a b c d (mihai a) (flory (a))))

;; 8.43
(defun flatten (tree)
  (cond ((null tree) nil)
        ((atom tree) (list tree))
        (t (append (flatten (car tree)) (flatten (cdr tree))))))

(flatten '())
(flatten '(a))
(flatten '(a b))
(flatten '(a (b)))
(flatten '((a) b))
(flatten '((a) (b)))
(flatten '((a b (r)) a c (a d ((a (b)) r) a)))
(flatten '((a b (r . z)) a c (a d ((a (b)) r) a)))

(cons (cons '(a . b) 'c) 'd)

;; 8.44
(defun tree-depth (tree)
  (cond ((atom tree) 0)
        (t (let ((d1 (1+ (tree-depth (car tree))))
                 (d2 (1+ (tree-depth (cdr tree)))))
             (if (> d1 d2) d1 d2)))))

(tree-depth '(a . b))
(tree-depth '(a b))
(tree-depth '(a b c d))
(tree-depth '((a b c d)))
(tree-depth '((b)))
(tree-depth '((a . b) (c . d)))
(tree-depth '((a . b) . (c . d)))

(cons (cons 'a 'b) (cons 'c 'd))
(cons (cons 'a 'b) (cons (cons 'c 'd) nil)) 

;; 8.45
(defun paren-depth (tree)
  (cond ((atom tree) 1)
        (t (let* ((left (car tree))
                  (right (cdr tree))
                  (counter (if (consp left) 1 0))
                  (d1 (+ counter (paren-depth left)))
                  (d2 (paren-depth right)))
             (if (> d1 d2) d1 d2)))))

(paren-depth '(a b c))
(paren-depth '(a (b) c))
(paren-depth '(a ((b)) c))
(paren-depth '(a b ((c) d) e))

; (cons (cons (cons ...))) -> 3 level
; (cons . (cons . (cons . ...) -> 1 level
; so, "cons" on the car position counts as 1 parenthesis
