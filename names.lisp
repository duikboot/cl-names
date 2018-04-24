;;;; names.lisp

(in-package #:names)


(defvar *original* nil)
(defvar *remaining* nil)

(defvar *db* '())

(defmacro while (test &body body)
 `(do ()
   ((not ,test))
   ,@body))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (random-elt set))

(defun take-n-names (n)
  (let ((names '()))
   (while (< (length names) n)
     (push (one-of *db*) names)
     (setf names (remove-duplicates names :test #'string-equal)))
   (sort names #'string-equal)))

(defun print-n-names (n)
  "Print n names with number"
  (let ((names (take-n-names n)))
    (terpri)
    (dotimes (i n)
      (format t "(~2d) ~A~%" (1+ i) (nth i names)))
    names))

(defun remove-from-db (item)
  "Remove name from database"
  (setf *db* (remove item *db* :test #'string-equal)))

(defun remove-but-n (n names)
  "Remove everything from names but the nth"
  (dotimes (i (length names))
    (when (not (= i n))
      (remove-from-db (nth i names))))
 t)

(defun print-choose-help()
  (format *query-io* "~%Choose a number to keep the name. You could also choose")
  (format *query-io* "~% <q> to quit the program")
  (format *query-io* "~%  0  to delete all names")
  (format *query-io* "~%Choose a number: ")
  (force-output *query-io*))

(defun choose (n)
 (let ((names (print-n-names n)))
   (print-choose-help)
   (let ((answer (read)))
     (if (integerp answer) (remove-but-n (1- answer) names) nil))))

(defun get-file (filename)
  "Read whole file"
  (with-open-file (stream filename :external-format :utf-8)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

; (define-condition database-file-not-available (error)
;   ((text :initarg :text :reader text)))

(defun help ()
  (princ
    "There has to be a 'girls-names.db', 'boys-names.db'  present in this directory. Also be sure to specify the number of names to process at a time.
")
  nil)

(defun load-database ()
  "Load file into memory."
  (if (probe-file *remaining*)
    (setf *db* (get-file *remaining*))
    (if (probe-file *original*)
      (setf *db* (get-file *original*))
      (help))))

(defun save-db (filename)
  "Save current *db* to file."
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :external-format :utf-8)
    (format out "~{~A~%~}" *db*)))


(defun format-totals ()
  (terpri)
  (terpri)
  (format t "================================~%")
  (format t "=~10d names left         =~%" (length *db*))
  (format t "================================~%")
  (terpri)
  (terpri))

(defun start (n gender)
  (alexandria:switch (gender :test #'string=)
    ("boy" (setf *original* "boys-names.db"
                 *remaining* "boys-results.db"))
    ("girl" (setf *original* "girls-names.db"
                  *remaining* "girls-results.db"))
    (t (help) (uiop:quit 1)))
  (when (load-database)
    (format-totals)
    (while (and (>= (length *db*) n) (choose n))
      (format-totals))
    (save-db *remaining*)
    (format-totals)))
