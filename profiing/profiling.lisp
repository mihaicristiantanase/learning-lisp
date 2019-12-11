;; Processes a CSV file and displays the duration of each action step
;; The format of an action (line) is: TIMESTAMP (SECONDS),ACTION,COMMENTS

(ql:quickload "parse-float")

(defun parse-line (line)
  (let ((tokens (split-sequence:split-sequence #\, line)))
    (list (parse-float:parse-float (first tokens) :type 'double-float :junk-allowed t)
          (second tokens))))

(defun parse (csv)
  (mapcar #'parse-line csv))

(defun show-durations-rec (parsed-csv)
  (when (> (length parsed-csv) 1)
    (let ((a1 (first parsed-csv))
          (a2 (second parsed-csv)))
      (format t "~&Duration of ~a:~49t ~5,3F"
              (second a1)
              (- (first a2) (first a1))))
    (show-durations-rec (rest parsed-csv))))

(defun show-durations (csv)
  (show-durations-rec (parse csv)))

(let ((csv '("1576076971.132421,startRegistrationTheOmyWay"
             "1576076971.132947,fetchPassword"
             "1576076994.611244,_orchestrator startActivation,I set the Pin here"
             "1576077004.009452,onActivationStepCompleteWithCommand"
             "1576077004.009578,sendCommandToServer"
             "1576077006.801086,onActivationStepCompleteWithCommand"
             "1576077006.801268,sendCommandToServer"
             "1576077007.986912,onActivationStepCompleteWithCommand"
             "1576077007.987056,sendCommandToServer"
             "1576077010.061297,onActivationStepCompleteWithCommand"
             "1576077010.061423,sendCommandToServer"
             "1576077027.631398,onActivationSuccess")))
  (show-durations csv))
