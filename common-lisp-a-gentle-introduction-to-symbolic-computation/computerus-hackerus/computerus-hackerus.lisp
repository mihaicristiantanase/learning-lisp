(defparameter nerdus-states
  '((sleeping . eating)
    (eating . waiting-for-a-computer)
    (waiting-for-a-computer . programming)
    (programming . debugging)
    (debugging . sleeping)))

(defun nerdus (state)
  (cdr (assoc state nerdus-states)))

(defun sleepless-nerdus (state)
  (let ((new-state (nerdus state)))
    (if (equal 'sleeping new-state)
      (nerdus new-state)
      new-state)
    ))

(defun nerdus-on-caffeine (state)
  (nerdus (nerdus state)))

(nerdus 'debugging)
(nerdus 'playing-guitar)
(sleepless-nerdus 'debugging)
(nerdus-on-caffeine 'debugging)

(defvar now-state 'programming)
(setf now-state (nerdus-on-caffeine now-state))
