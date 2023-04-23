(ql:quickload "drakma")

(defpackage :spacy-client
   (:use :cl :cl-user :drakma))

(in-package :spacy-client)

(defparameter *url*  "http://127.0.0.1:8008")

(defun test (query)
  "Documentation for test1 with parameters "
  (format t "~&~%Testing ~a~%" query)

  (format t "~&~a~%"
          (flexi-streams:octets-to-string
           (http-request *url*
                         :parameters
                         `(("text" . ,query))))))

(defun test1 ()
  (test "Mih is here with Flo. She loves him. She is younger."))

(defun test2 ()
  (test "What is going on with Romania?   Is anyone listening?"))

(defun test3 ()
  (test "To be or not to be Michael. Red, blue and green. I am green."))

(progn
  (test1)
  (test2)
  (test3))
