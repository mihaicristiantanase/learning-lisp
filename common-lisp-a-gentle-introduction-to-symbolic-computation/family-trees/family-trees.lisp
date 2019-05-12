;;; Weird family trees

(defvar family
  '((colin nil nil)
    (deirdre nil nil)
    (arthur nil nil)
    (kate nil nil)
    (frank nil nil)
    (linda nil nil)
    (suzanne colin deirdre)
    (bruce arthur kate)
    (charles arthur kate)
    (david arthur kate)
    (ellen arthur kate)
    (george frank linda)
    (hillary frank linda)
    (andre nil nil)
    (tamara bruce suzanne)
    (vincent bruce suzanne)
    (wanda nil nil)
    (ivan george ellen)
    (julie george ellen)
    (marie george ellen)
    (nigel andre hillary)
    (frederick nil tamara)
    (zelda vincent wanda)
    (joshua ivan wanda)
    (quentin nil nil)
    (robert quentin julie)
    (olivia nigel marie)
    (peter nigel marie)
    (erica nil nil)
    (yvette robert zelda)
    (diane peter erica)))

(defun father (p)
  (second (assoc p family)))

(defun mother (p)
  (third (assoc p family)))

(defun parents (p)
  (remove-if #'(lambda (x) (null x))
             (rest (assoc p family))))

(defun children (p)
  (if (null p) nil
    (mapcar #'first
            (remove-if-not #'(lambda (x) (member p (rest x))) family))))

(defun mapunion (f l)
  (let ((res (mapcar f l)))
    (if (> (length res) 0)
      (reduce #'union res)
      res)))

(defun siblings (p)
  (remove p (mapunion #'children (parents p))))

(defun grandparents (p)
  (mapunion #'parents (parents p)))

(defun cousins (p)
  (mapunion #'children
            (mapunion #'siblings (parents p))))

(defun descended-from (x y)
  (let ((prnts (parents x)))
    (cond ((null prnts) nil)
          ((member y prnts) t)
          (t (or (descended-from (first prnts) y)
                 (descended-from (second prnts) y))))))

(defun ancestors (p)
  (let ((prnts (parents p)))
    (cond ((null prnts) nil)
          (t (append prnts
                     (ancestors (first prnts))
                     (ancestors (second prnts)))))))

(defun generation-gap-rec (x y)
    (let ((prnts (parents x)))
      (cond ((null prnts) 0)
            ((member y prnts) 1)
            (t (max (1+ (generation-gap-rec (first prnts) y))
                    (1+ (generation-gap-rec (second prnts) y)))))))

(defun generation-gap (x y)
  (if (not (descended-from x y))
    nil
    (generation-gap-rec x y)))

;; tests

(father 'tamara)
(mother 'tamara)
(parents 'tamara)
(parents 'frederick)
(parents 'colin)
(children 'tamara)
(children 'arthur)
(siblings 'charles)
(siblings 'zelda)
(siblings 'wanda)
(father nil)
(mother nil)
(parents nil)
(children nil)
(mapunion #'rest '((1 a b c) (2 e c j) (3 f a b c d)))
(grandparents 'diane)
(grandparents 'joshua)
(grandparents 'robert)
(grandparents 'yvette)
(cousins 'julie)
(cousins 'joshua)
(descended-from 'tamara 'arthur)
(descended-from 'tamara 'linda)
(ancestors 'marie)
(ancestors 'diane)
(ancestors 'nigel)
(ancestors 'julie)
(generation-gap 'suzanne 'colin)
(generation-gap 'frederick 'colin)
(generation-gap 'frederick 'linda)

;; questions

(descended-from 'robert 'deirdre)
(ancestors 'yvette)
(generation-gap 'olivia 'frank)
(cousins 'peter)
(grandparents 'olivia)
