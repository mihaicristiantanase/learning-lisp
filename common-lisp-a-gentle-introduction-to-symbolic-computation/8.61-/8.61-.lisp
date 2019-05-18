;; 8.61

(defun count-up-tr (n acc)
  (cond ((zerop n) acc)
        (t (count-up-tr (1- n) (cons n acc)))))
(defun count-up (n)
  (count-up-tr n '()))

(count-up 5)
(count-up 1)

;; 8.62
(defun fact-tr (n acc)
  (cond ((zerop n) acc)
        (t (fact-tr (1- n) (* n acc)))))
(defun fact (n)
  (fact-tr n 1))

(fact 1)
(fact 3)
(fact 5)

;; 8.63

; union
(defun my-union-tr (x acc)
  (cond ((null x) acc)
        (t (if (member (first x) acc)
             (my-union-tr (rest x) acc)
             (my-union-tr (rest x) (cons (first x) acc))))))

(defun my-union (x y)
  (my-union-tr y (my-union-tr x '())))

(my-union '(1 2 3) '(a b 1 2))

; intersection
(defun my-intersection-tr (x y acc)
  (cond ((null x) acc)
        (t (if (member (first x) y)
             (my-intersection-tr (rest x) y (cons (first x) acc))
             (my-intersection-tr (rest x) y acc)))))

(defun my-intersection (x y)
  (my-intersection-tr x y '()))

(my-intersection '(1 2 3) '(a b 1 2))

; set-difference
(defun my-set-difference-tr (x y acc)
  (cond ((null x) acc)
        (t (if (member (first x) y)
             (my-set-difference-tr (rest x) y acc)
             (my-set-difference-tr (rest x) y (cons (first x) acc))))))

(defun my-set-difference (x y)
  (my-set-difference-tr x y '()))

(my-set-difference '(1 2 3) '(a b 1 2))

;; 8.64
(defun tree-find-if (fn tree)
  (cond ((and tree (atom tree) (funcall fn tree)) tree)
        ((atom tree) nil)
        (t (or (tree-find-if fn (car tree))
               (tree-find-if fn (cdr tree))))))

(tree-find-if #'oddp '((4 5) (6 7)))
(tree-find-if #'oddp '((2 4) (6 7)))

;; 8.65

; count slices
(defun tr-count-slices (x)
  (labels ((tr (x acc)
               (cond ((null x) acc)
                     (t (tr (rest x) (1+ acc))))))
    (tr x 0)))

(tr-count-slices '(1 2 a 4))
(tr-count-slices '(a 4))

; reverse
(defun tr-reverse (x)
  (labels ((rec (x acc)
                (if x
                  (rec (rest x) (cons (first x) acc))
                  acc)))
    (rec x '())))

(tr-reverse '(1 2 3 9))

;; 8.66
(defun arith-eval (expr)
  (cond ((numberp expr) expr)
        (t (let ((op1 (arith-eval (first expr)))
                 (op (second expr))
                 (op2 (arith-eval (third expr))))
             (cond ((equal '+ op) (+ op1 op2))
                   ((equal '- op) (- op1 op2))
                   ((equal '* op) (* op1 op2))
                   ((equal '/ op) (/ op1 op2))
                   (t 0))))))

(arith-eval '(2 + (3 * 4)))
