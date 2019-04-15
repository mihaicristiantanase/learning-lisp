(defparameter states
  '((pennsylvania (pittsbburgh johnstown))
    (new-jersey (newark princeton trenton))
    (ohio (columbus))))

(defun cities-in (state)
  (second (assoc state states)))

(cities-in 'pennsylvania)
