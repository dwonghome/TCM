(defun hello-world ()
  (format t "hello, world"))

(defun test (x)
  (list (hello-world) x))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record (cd) (push cd *db*))

(defun dump-db (db)
  (dolist (cd db)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defvar *herb-db* nil)
(defun make-herb (name temp flavor indications rating freq comment)
  (list :name name :temp temp :flavor flavor :indications indications 
	:rating rating :freq freq :comment comment))
(defun add-herb (herb) (push herb *herb-db*))

(defvar *point-db* nil)
(defun make-point (name type location danger indications contraindications rating freq comment)
  (list :name name :type type :location location :danger danger 
	:indications indications :contraindications contraindications
	:rating rating :freq freq :comment comment))
(defun add-point (point) (push point *point-db*))
(defun make-indications (effective must-have general)
  (list :effective effective :must-have must-have :general general))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-read-list (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (let ((r nil))
  (loop (let ((line (read-line *query-io*)))
      (setf r (cons line r))
      (if (equal line "") (return (reverse (cdr r))))))))


(defun save-db (db filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print db out))))

(defun load-db (db filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (set db (read in)))))
(defun load-point-db (filename)
  (load-db '*point-db* filename))
(defun load-herb-db (filename)
  (load-db '*herb-db* filename))


(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
      (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun prompt-for-indications ()
  (make-indications
   (prompt-read-list "Effective")
   (prompt-read-list "Must Have")
   (prompt-read-list "General")))
(defun prompt-for-point ()
  (make-point
   (prompt-read "Name")
   (prompt-read-list "Type")
   (prompt-read "Location")
   (prompt-read-list "Danger")
   (prompt-for-indications)
   (prompt-read-list "Contraindications")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (or (parse-integer (prompt-read "Frequency") :junk-allowed t) 0)
   (prompt-read "Comment")))
(defun prompt-add-points ()
  (loop (add-point (prompt-for-point))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun select (db selector-fn)
  (remove-if-not selector-fn db))
(defun name-selector (name)
  #'(lambda (point-herb) (equal (getf point-herb :name) name)))
(defun indications-selector (name)
  #'(lambda (point-herb) (contains name (getf point-herb :indications))))
(defun effective-selector (name)
  #'(lambda (point-herb) (contains name (getf (getf point-herb :indications) :effective))))

(defun contains (name list)
  (dolist (x list)
       (if (or (if (typep x 'list) (contains name x))
	   (if (equal x name) 'T)) 
	   (return 'T))))


(union '(a b) '(b c))
(intersection '(a b) '(b c))
