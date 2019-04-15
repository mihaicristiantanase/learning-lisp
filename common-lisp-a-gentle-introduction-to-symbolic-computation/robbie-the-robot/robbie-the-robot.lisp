;;; ROBBIE THE ROBOT

(defvar rooms
  '((living-room
      (north front-stairs)
      (south dining-room)
      (east kitchen))
    (upstairs-bedroom
      (west library)
      (south front-stairs))
    (dining-room
      (north living-room)
      (west downstairs-bedroom)
      (east pantry))
    (kitchen
      (west living-room)
      (south pantry))
    (pantry
      (north kitchen)
      (west dining-room))
    (downstairs-bedroom
      (north back-stairs)
      (east dining-room))
    (back-stairs
      (south downstairs-bedroom)
      (north library))
    (front-stairs
      (north upstairs-bedroom)
      (south living-room))
    (library
      (east upstairs-bedroom)
      (south back-stairs))))

(defvar loc 'pantry)

(defun choices (room)
  (rest (assoc room rooms)))

(defun look (dir room)
  (second (assoc dir (choices room))))

(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting the variable LOC."
  (setf loc place))

(defun how-many-choices ()
  (length (choices loc)))

(defun upstairp (place)
  (if (member place '(upstairs-bedroom library)) t))

(defun onstairp (place)
  (if (member place '(front-stairs back-stairs)) t))

(defun where ()
  (append '(robbie is)
          (cond ((upstairp loc) (list 'upstairs 'in 'the loc))
                ((onstairp loc) (list 'on 'the loc))
                (t (list 'downstairs 'in 'the loc)))))

(defun move (dir)
  (let ((place (look dir loc)))
    (if place
      (progn
        (set-robbie-location place)
        (where))
      '(ouch! robbie hit a wall))))

(choices 'pantry)
(choices 'front-stairs)

(look 'north 'pantry)
(look 'south 'library)

loc

(how-many-choices)

(upstairp 'upstairs-bedroom)
(onstairp 'front-stairs)

(set-robbie-location 'pantry)
(where)
(move 'north)
(move 'west)
(move 'north)
(move 'north)
(move 'west)

;; only the last cell is returned!
;; but everything is executed!!!!!
(cond ((equal 3 3) '(this is a test) '(and another one))
      (t nil))
(defparameter a 1)
(defparameter b 2)
(cond ((equal 3 3) (setf a 10) (setf b 11) '(and another one))
      (t nil))
a
b
