;;; OrangeMoney WebServer Mock
;; TODO(mihai): fix this
;; PROCESS-PRESET: Calling a multiprocessing function on a single-threaded sbcl build

(require :aserve)

(defpackage :orange-money
    (:use :common-lisp :net.aserve :net.html.generator))

(in-package :orange-money)

(start :port 8001)
