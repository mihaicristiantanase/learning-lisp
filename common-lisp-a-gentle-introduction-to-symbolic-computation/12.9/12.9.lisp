;; 12.9
(defstruct starship
  (captain nil)
  (name nil)
  (shields 'down)
  (condition 'green)
  (speed 0))

(defun print-captain (x stream depth)
  (format stream "#<CAPTAIN ~A>" (captain-name x)))

(defstruct (captain
             (:print-function print-captain))
  (name nil)
  (age nil)
  (ship nil))

(defvar james-kirk (make-captain :name "James T. Kirk" 
                                 :age 35
                                 :ship "Enterprise"))
(defvar enterprise (make-starship :captain james-kirk
                                  :name "Enterprise"))

(format t "~A~%" enterprise)
