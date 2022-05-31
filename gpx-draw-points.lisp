;;;; Add linear interpolation between GPS coordinates to make it easier to see patterns

;; 1. Example of file
;;
;; name,description,latitude,longitude
;; point0,point0,44.48429765789908,26.099893381588515
;; point1,point1,44.484296106052426,26.099827894036167
;; point2,point2,44.484497342957724,26.099818934327857
;; point3,point3,44.48438340507175,26.099241954158032
;; point4,point4,44.48421525901347,26.09924952628603
;; point5,point5,44.484215775789835,26.0992645798662
;; point6,point6,44.48422568175167,26.09945742282565
;; point7,point7,44.48417933581692,26.099462134347874
;; point8,point8,44.48416906639173,26.099268787672344
;; point9,point9,44.48417164534848,26.0992511882838
;; point10,point10,44.484169774564464,26.099186378482898
;; point11,point11,44.48439772974308,26.099175509767605
;; point12,point12,44.48452124598438,26.099818689713885
;; point13,point13,44.484522302733296,26.099883006643324
;; point14,point14,44.48429765789908,26.099893381588515

;; 2. To see the result
;;
;; https://www.gpsvisualizer.com/convert_input (with GPX selected)

(ql:quickload "cl-ppcre")

(defparameter *fname* #p"/tmp/capalna-x-prometeu.txt")
(defparameter *gps* "^([^,]+),([^,]+),([^,]+),([^,]+)")
(defparameter *steps* 10)

(defun to-number (str)
  (let ((rv (with-input-from-string (s str) (read s))))
    (if (numberp rv) rv nil)))

(defun lerp (l r step)
  (+ l (* (- r l) step)))

(setf *read-default-float-format* 'double-float)

(let (prev-name
      prev-descr
      prev-lat
      prev-lng)
  (with-open-file (f *fname*)
    (loop for line = (read-line f nil nil)
          while line
          do (cl-ppcre:register-groups-bind (name descr lat lng)
                 (*gps* line)
               (setf lat (to-number lat)
                     lng (to-number lng))
               (when prev-lat
                 (loop for step below *steps*
                       do (format t "~a,~a,~a,~a~%"
                                  prev-name
                                  prev-descr
                                  (lerp prev-lat lat (/ step *steps*))
                                  (lerp prev-lng lng (/ step *steps*)))))
               (format t "~a~%" line)
               (setf prev-name name
                     prev-descr descr
                     prev-lat lat
                     prev-lng lng)))))
