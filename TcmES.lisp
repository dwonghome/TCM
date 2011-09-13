(defun dump-db (db)
  (dolist (cd db)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defvar *herb-db* nil)
(defun make-herb (name temp flavor indications contraindications rating freq comment)
  (list :name name :temp temp :flavor flavor 
	:indications indications :contraindications contraindications 
	:rating rating :freq freq :comment comment))
(defun add-herb (herb) (push herb *herb-db*))


(defvar *herb-formula-db* nil)
(defun make-herb-formula (name indications contraindications ingredients instruction reference rating freq comment)
  (list :name name :indications indications :contraindications contraindications
	:ingredients ingredients :instruction instruction :reference reference 
	:rating rating :freq freq :comment comment))
(defun add-herb-formula (herb-formula) (push herb-formula *herb-formula-db*))
; ingredients are list of (("Herb name" doze) ("MaHuang" 2)...)


(defvar *point-db* nil)
(defun make-point (name type location danger indications contraindications instruction reference rating freq comment)
  (list :name name :type type :location location :danger danger 
	:indications indications :contraindications contraindications
	:instruction instruction :reference reference
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
   (prompt-read "Instruction")
   (prompt-read "Reference")
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
(defun selectf (db selector-fn field)
  (loop for x in (select db selector-fn)
    collect (getf x field)))

; for future ref
;(union '(a b) '(b c))
;(intersection '(a b) '(b c))

(defun get-name (herb-doze)
  (car herb-doze))
(defun get-doze (herb-doze)
  (car (cdr herb-doze)))
(defun calc-herb (herbs-dozes g-per-day times-per-day num-days)
  (let ((s 0) (a (* g-per-day times-per-day num-days)))
    (dolist (hd herbs-dozes)
      (setf s (+ (get-doze hd) s)))
    (setf s (/ a s))
    (dolist (hd herbs-dozes)
      (format t "~a:~20t~5,1f~%" (get-name hd) (float (* (get-doze hd) s))))
    (format t "~a:~20t~5,1f~%" "Total" a)))

;example usage
;(calc-herb '((a 1) (b 2) (c 3)) 6 2 5)
;(calc-herb '(("MaHuang" 10) ("GuiZhi" 10) ("ZhiGanCao" 10)) 6 2 5)

(defun get-herb-formula (name)
  (car (select *herb-formula-db* (name-selector name))))
(defun calc-herb-formula (name g-per-day times-per-day num-days)
  (calc-herb (getf (get-herb-formula name) :ingredients) g-per-day times-per-day num-days))

;example usage
;(calc-herb-formula "Ma Huang Tang" 6 2 10)
;(calc-herb-formula "Gui Zhi Tang" 6 2 10)

