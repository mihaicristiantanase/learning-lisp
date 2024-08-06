(ql:quickload "cl-cairo2")

(defpackage :icons
  (:use :cl
        :cl-cairo2))

(in-package :icons)

(defun draw-text (text &key (size 190)
                            (color '(1 1 1))
                            x
                            y)
  (apply #'set-source-rgb color)
  (move-to x y)
  (select-font-face "Apple Color Emoji" :normal :normal)
  ;; (select-font-face  "Arial Unicode MS" :normal :normal)
  (set-font-size size)
  (show-text text))

(defun draw-rect (x y w h r g b)
  (set-source-rgba r g b 1.0)
  (rectangle x y w h)
  (fill-path))

(defun create-icon (name text w h)
  "Documentation for create-icon with parameters text"
  (ensure-directories-exist name)
  (with-png-file (name :argb32 w h)
    (draw-text text :x 0 :y (- h 10)))
  ;; TODO(mihai): don't use imagemagick to cleanup the images
  (uiop:run-program (format nil "convert -trim ~a ~a" name name))
  (let ((img-surface (image-surface-create-from-png name)))
    (with-png-file (name :argb32 w h)
      ;; (draw-rect 0 0 w h 0 0 0)
      (let ((img-w (width img-surface))
            (img-h (height img-surface)))
        (set-source-surface img-surface (/ (- w img-w) 2) (/ (- h img-h) 2))
        (paint)))))

(let ((icons '((blur . "BL")
               (tint . "TI")
               (brightness . "BR")
               (pushpin . "📌")
               (round-pushpin . "📍")
               (bullseye . "◎")
               (position-indicator . "⌖")
               ))
      (w 220)
      (h 220))
  (loop for icon in icons
        for img-name = (format nil "/tmp/icons/icon_~(~a~).png" (car icon)) do

          (create-icon img-name (cdr icon) w h))
  (prin1 'done))
