;; Discrimination net for automotive diagnosis

(defstruct node
  (name nil)
  (question nil)
  (yes-case nil)
  (no-case nil))

(defparameter *node-list* nil)

(defun init () (setf *node-list* '()))

(defun add-node (name question yes-case no-case)
  (let ((node (make-node :name name
                         :question question
                         :yes-case yes-case
                         :no-case no-case)))
    (push node *node-list*)
    node))

(defun find-node (name)
  (dolist (nd *node-list* nil)
          (when (equal name (node-name nd)) (return nd))))

(defun process-node (name)
  (let ((nd (find-node name)))
    (if (null nd)
      (format t "Node ~A is not defined" name)
      (if (yes-or-no-p (node-question nd))
        (node-yes-case nd)
        (node-no-case nd)))))

(defun run ()
  (let ((current-node 'start))
    (labels ((run-rec (nd)
                      (let ((resp (process-node nd)))
                        (cond ((null resp) nil)
                              ((symbolp resp) (run-rec resp))
                              ((stringp resp) (format t resp))
                              (t nil)))))
      (run-rec current-node))))

(defun add-node-interactively ()
  ;; TODO(mihai): fix this
  )

;; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
(init)
(add-node 'start
          "Does the entine turn over?"
          'engine-turns-over
          'engine-wont-turn-over)
(add-node 'engine-turns-over
          "Will the engine run for any period of time?"
          'engine-will-run-briefly
          'engine-wont-run)
(add-node 'engine-wont-run
          "Is there gas in the tank?"
          'gas-in-tank
          "Fill the tank and try starting the engine again.")
(add-node 'engine-wont-turn-over
          "Do you hear any sound when you turn the key?"
          'sound-when-turn-key
          'no-sound-when-urn-key)
(add-node 'no-sound-when-urn-key
          "Is the battery voltage low?"
          "Replace the battery"
          'battery-voltage-ok)
(add-node 'battery-voltage-ok
          "Are the battery cables dirty or loose?"
          "Clean the cables and tighten the connections."
          'battery-cables-good)
(add-node 'engine-will-run-briefly
          "Does the engine stall when cold but not when warm?"
          'engine-stall-when-cold
          "Adjust idle speed")
(add-node 'engine-stall-when-cold
          "Is the cold speed at least 700 rpm?"
          "Good!"
          "Increase rpm.")

(find-node 'no-sound-when-urn-key)

(process-node 'no-sound-when-urn-key)

(run)
