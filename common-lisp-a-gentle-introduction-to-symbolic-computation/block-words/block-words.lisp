;;; Block words

(defvar database
  '((b1 shape brick)
    (b1 color green)
    (b1 size small)
    (b1 material wood)
    (b1 supported-by b2)
    (b1 supported-by b3)
    (b2 shape brick)
    (b2 color red)
    (b2 size small)
    (b2 material plastic)
    (b2 supports b1)
    (b2 left-of b3)
    (b3 shape brick)
    (b3 color red)
    (b3 size small)
    (b3 supports b1)
    (b3 right-of b2)
    (b4 shape pyramid)
    (b4 color blue)
    (b4 size large)
    (b4 supported-by b5)
    (b5 shape cube)
    (b5 color green)
    (b5 size large)
    (b5 supports b4)
    (b6 shape brick)
    (b6 color purple)
    (b6 size large)))

(defun match-element (x y)
  (or (equal x y) (equal y '?)))

(defun match-triple (assertion pattern)
  (every #'match-element assertion pattern))

(defun fetch (pattern)
  (remove-if-not
    #'(lambda (assertion) (match-triple assertion pattern))
    database))

(defun color-pattern (block)
  (list block 'color '?))

(defun supports-pattern (block)
  (list block 'supports '?))

(defun supported-by-pattern (block)
  (list block 'supported-by '?))

(defun shape-pattern (block)
  (list block 'shape '?))

(defun is-cube (block)
  (> (length (fetch (list block 'shape 'cube))) 0))

(defun supporters (block)
  (mapcar #'third (fetch (supported-by-pattern block))))

(defun supp-cube (block)
  (if (find-if #'is-cube (supporters block)) t))

(defun desc1 (block)
  (fetch (list block '? '?)))

(defun desc2 (block)
  (mapcar #'rest (desc1 block)))

(defun description (block)
  (reduce #'append (desc2 block)))

(defun all-properties ()
  (reduce #'union
          (mapcar #'(lambda (block) (list (second block))) database)))

(match-element 'red 'red)
(match-element 'red '?)

(match-triple '(b2 color red) '(b2 color red))
(match-triple '(b2 color red) '(b2 color ?))
(match-triple '(b1 color red) '(b2 color red))

(fetch '(b2 color ?))
(fetch '(? supports b2))
(fetch '(? supports b1))

(fetch '(b4 shape ?))
(fetch '(? shape brick))
(fetch '(b2 ? b3))
(fetch '(? color ?))
(reduce #'union (mapcar #'last (fetch '(? color ?))))
(fetch '(b4 ? ?))

(color-pattern 'b3)

(supporters 'b1)

(is-cube 'b1)

(supp-cube 'b3)

(desc1 'b2)
(desc2 'b2)
(description 'b5)
(description 'b1)

(all-properties)
