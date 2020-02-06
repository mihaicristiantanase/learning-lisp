;;; Process 'jpg' and 'heic' images
;;;
;;; Use lowercased extensions
;;; Fix image rotation
;;; Transform heic to jpg
;;; Read image metadata and extract the real creation date

(load "~/.sbclrc")
(ql:quickload :cl-ppcre)

(defparameter *extensions* '("jpeg" "jpg" "heic"))

(defmacro with-check (func-name &body body)
  `(labels ((check (condition &optional (error-message nil))
              (when (not condition)
                (when error-message (format t "~&[Error] ~a.~%" error-message))
                (return-from ,func-name nil))))
     ,@body))

(defun add-spaces-between (lst)
  (when (not (null lst))
    (append (list " " (car lst)) (add-spaces-between (rest lst)))))

(defun run-shell (&rest cmd-parts)
  (let* ((cmd (uiop/launch-program:escape-sh-command cmd-parts)))
    (format t "~&$ ~A" cmd)
    (let* ((shell-output (with-output-to-string (s)
                           (sb-ext:run-program "/bin/bash" (list "-c" cmd) :input nil :output s) s))
           (lines (uiop/utility:split-string shell-output :separator '(#\Newline))))
      (remove "" lines :test #'string=))))

(defun valid-directory (path)
  (let ((pf (probe-file path)))
    (when pf (not (pathname-name pf)))))

(defun list-images (path)
  (remove-if-not (lambda (f) (find (pathname-type f) *extensions* :test 'string-equal))
                 (uiop:directory-files path)))

(defun extract-orientation (line)
  (let ((last (cadr (uiop/utility:split-string line :separator '(#\:)))))
    (when last (string-trim '(#\Space) last))))

(defparameter creation-date-scanner
  (cl-ppcre:create-scanner "exif:DateTimeOriginal: (\\d+):(\\d+):(\\d+) (\\d+):(\\d+):(\\d+)"))

(defun extract-creation-date (line)
  (multiple-value-bind (_ match)
      (cl-ppcre:scan-to-strings creation-date-scanner line)
    (declare (ignore _))
    (when match
      (parse-integer (format nil "~{~a~}" (coerce match 'list))))))

(defun valid-convert-response (output)
  (null output))

(defun fix-extension (path)
  (format t "~&> Fix extension~%")
  (dolist (f (list-images path))
    (let* ((img (pathname f))
           (ext (pathname-type f))
           (img-new (make-pathname
                         :defaults img
                         :type (if (string-equal ext "jpeg") "jpg" (string-downcase ext)))))
      (when (not (equal img img-new)) (rename-file img img-new))))
  't)

(defun fix-orientation (path)
  (format t "~&> Fix orientation~%")
  (with-check fix-orientation
    (dolist (img (list-images path))
      (let* ((imgname (file-namestring img))
             (shell-output (run-shell "identify" "-verbose" (namestring img)))
             (orientation-line (find-if
                                 (lambda (line) (search "orientation" line :test #'string-equal))
                                 shell-output))
             (orientation (extract-orientation orientation-line)))
        (check orientation (format nil "Could not obtain orientation of ~a" imgname))
        (when (not (string-equal orientation "TopLeft" ))
          (format t "~&Fixing orientation for ~a having ~a~%" imgname orientation)
          (check (valid-convert-response
                   (run-shell "convert" (namestring img) "-auto-orient" (namestring img)))
                 (format nil "Could not fix orientation of ~a" imgname))))))
  't)

(defun convert-heic (path)
  (format t "~&> Convert HEIC~%")
  (with-check convert-heic
    (dolist (img (remove-if-not
                   (lambda (img) (string-equal (pathname-type img) "heic"))
                   (list-images path)))
      (let* ((imgname (file-namestring img))
             (img-new (make-pathname :defaults img :type "jpg"))
             (shell-output (run-shell "convert" (namestring img) (namestring img-new))))
        (check (valid-convert-response shell-output)
               (format nil "Could not convert ~a" imgname))
        (delete-file img))))
  't)

(defun extract-dates (path)
  (with-check extract-dates
    (let ((dates '()))
      (dolist (img (list-images path))
        (let* ((imgname (file-namestring img))
               (shell-output (run-shell "identify" "-verbose" (namestring img)))
               (creation-date-line
                 (find-if
                   (lambda (line) (search "exif:DateTimeOriginal" line :test #'string-equal))
                   shell-output))
               (creation-date (extract-creation-date creation-date-line)))
          (push (cons img creation-date) dates)
          (check creation-date (format nil "Could not obtain creation date of ~a" imgname))))
      dates)))

(defun generate-order-by-time (path)
  (let ((dates (extract-dates path)))
    (if (member nil dates) nil dates)))

(defun compute-new-name (indexed-img)
  (let* ((img (cdr indexed-img))
         (img-name (pathname-name img))
         (prefix (format nil "~5,'0d" (+ 1 (car indexed-img))))
         (suffix (if (search "DSC" img-name :test #'string=) "DSC" "IMG"))
         (img-new (make-pathname
                    :defaults img
                    :name (format nil "~a_~a" prefix suffix))))
    (cons (cdr indexed-img) img-new)))

(defun compute-new-names (imgs)
  (mapcar #'compute-new-name (loop for index from 0
                                   and img in imgs
                                   collecting (cons index img))))

(defun rename-temporary (path)
  (format t "~&> Rename temporary to avoid name conflicts~%")
  (dolist (img (list-images path))
    (let* ((img-new (make-pathname
                      :defaults img
                      :name (format nil "__~a" (pathname-name img)))))
      (rename-file img img-new)))
  't)

(defun rename-to-match-time (path)
  (format t "~&> Rename to match time~%")
  (with-check rename-to-match-time
    (let ((dates (generate-order-by-time path)))
      (check dates)
      (dolist (rename-tuple (compute-new-names (mapcar #'car (sort dates #'< :key #'cdr))))
        (rename-file (car rename-tuple) (cdr rename-tuple)))))
  't)

(defun process-imgs (path)
  (cond
    ((not (valid-directory path)) (format t "~&[Error] valid-directory~%"))
    ((not (fix-extension path)) (format t "~&[Error] fix-extension~%"))
    ((not (fix-orientation path)) (format t "~&[Error] fix-orientation~%"))
    ((not (convert-heic path)) (format t "~&[Error] convert-heic~%"))
    ((not (rename-temporary path)) (format t "~&[Error] rename-temporary~%"))
    ((not (rename-to-match-time path)) (format t "~&[Error] rename-to-match-time~%"))
    ('t)))

(defun process-args (args)
  (cond
    ((not (equal (length args) 2))
     (format t "~&[Error] Should run as ~A <folder_images>.~%" (first args)))
    ((not (process-imgs (cadr args)))
     (format t "~&[Error] The processing was incomplete.~%"))
    ('t (format t "~&The processing was successful.~%"))))

(defun main (&optional (args *posix-argv*)) (process-args args))

(main)
