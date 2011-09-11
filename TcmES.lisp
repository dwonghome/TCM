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
(defun make-herb (name temp flavor indications contraindications rating freq comment)
  (list :name name :temp temp :flavor flavor 
	:indications indications :contraindications contraindications 
	:rating rating :freq freq :comment comment))
(defun add-herb (herb) (push herb *herb-db*))

(defvar *herb-formula-db* nil)
(defun make-herb-formula (name indications contraindications ingredients rating freq comment)
  (list :name name :indications indications :contraindications contraindications
	:ingredients ingredients :rating rating :freq freq :comment comment))
(defun add-herb-formula (herb-formula) (push herb-formula *herb-formula-db*))
; ingredients are list of (("Herb name" doze) ("MaHuang" 2)...)

(add-herb-formula (make-herb-formula "Gui Zhi Tang" 
				     '(:effective () :must-have ("headache" "feeling cold" "sweat") :general ("Tai Yang Bing")) '("no sweat")
				     '(("Gui Zhi" 6) ("Bai Shao" 6) ("Sheng Jiang" 4) ("Zhi Gan Cao" 4) ("Dai Zao" 4)) 9 0 ""))
(add-herb-formula (make-herb-formula "Ma Huang Tang" 
				     '(:effective () :must-have ("headache" "feeling cold" "no sweat") :general ("Tai Yang Bing")) '("sweat")
				     '(("Ma Huang" 4) ("Gui Zhi" 6) ("Xing Ren" 3) ("Zhi Gan Cao" 4)) 9 0 ""))

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

(calc-herb '((a 1) (b 2) (c 3)) 6 2 5)
(calc-herb '(("MaHuang" 10) ("GuiZhi" 10) ("ZhiGanCao" 10)) 6 2 5)

(defun get-herb-formula (name)
  (car (select *herb-formula-db* (name-selector name))))
(defun calc-herb-formula (name g-per-day times-per-day num-days)
  (calc-herb (getf (get-herb-formula name) :ingredients) g-per-day times-per-day num-days))

(calc-herb-formula "Ma Huang Tang" 6 2 10)
(calc-herb-formula "Gui Zhi Tang" 6 2 10)

(defvar *5elements-db* nil)
(setf *5elements-db* '(
		       (:name "Wood" :property '("life" "excite"))
		       (:name "Fire" :property '("change" "movement"))
		       (:name "Earth" :property '("give birth" "nurture"))
		       (:name "Metal" :property '("regulate" "rule"))
		       (:name "Water" :property '("hidden" "end"))))

(defvar *Trigrams* '(Qian Kun Zhen Xun Kan Li Gen Dui))
(defvar *Trigrams-db* 
  '((:name Qian :yin-yang Yang :5elements Metal 
     :nature Sky :season Autumn-Winter :direction NW :animal Horse :family "Father" :body Head 
     :tendency '(Strong-Move) :weather Sunny :organ '(LI) :ZhiQi NaDong :color '(red) 
     :others '("nation" "King" "Jade" "Gold" "Cold" "Ice" "Wood-Fruit")
     :diseases '("head" "chest" "bone pain" "harden" "chronic" "wind-cold" "abnormal change" "acute" "blockage" 
		 "constipation" "nerve" "stroke"))
    (:name Kun :yin-yang Yin :5elements Earth 
     :nature Earth :season Summer-Autumn :direction SW :animal Cow :family "Mother" :body Abdomen 
     :tendency '(Follow Gental Stingy) :weather Cloud :organ '(SP) :ZhiQi NaChao :color '(black-soil) 
     :others '("cloth" "axe" "car" "text" "crowd" "handle")
     :diseases '("abdomen" "GI" "digestion" "swollen" "dampness" "skin" "blister" "faint" "fatique" "chronic" 
		 "middle Qi deficient" "cancer"
		 "weak muscle" "distension" "serious severe disease" "dieing"))
    (:name Zhen :yin-yang Yang :5elements Wood 
     :nature Thunder :season Spring :direction E :animal Dragon :family "Eldest Son" :body Leg 
     :tendency '(Start-Move Give Decisive) :weather Thunder :organ '(LIV) :ZhiQi ChungFan :color '(black-yellow) 
     :others '("big road" "young bamboo" "root-fruit" "pennut" "yam" "potato")
     :diseases '("mental" "manic" "sensitive" "epilapsy" "nervious" "hyeractive" "lady problem" 
		 "liver fire" "pain" "swollen leg" "tramma" "acute" 
		 "servere" "cough" "vocal" "throat" "liver" "fatty liver" "hepititis"))
    (:name Xun :yin-yang Yin :5elements Wood 
     :nature Wind :season Spring-Summer :direction SE :animal Chicken :family "Eldest Daughter" :body Buttock 
     :tendency '(inward) :weather Wind :organ '(GB) :ZhiQi NaXia :color '(white)
     :others '("vertial rob" "work" "long" "high" "advance" "drawback" "no fruit" "odor" "bold" "white eye")
     :diseases '("wind" "stroke" "mental" "gall bladder" "infection" "sciatia" "lymph" "cramp" "stiff" 
		 "unstable" "dyspnea" "asthema" "left shoulder pain"
		 "nerve inflammation" "hip pain" "chest distention" "drunk" "abdomen distention" "depression" 
		 "blood vessel" "gall stone" "gall bladder infection" "bile duct"))
    (:name Kan :yin-yang Yang :5elements Water 
     :nature Water :season Winter :direction N :animal Pig :family "Middle Son" :body Ear 
     :tendency '(descend danger hide gental-beautiful worry) :weather Rain :organ '(K BL) :ZhiQi DongZhi :color '()
     :others '("sewage" "car" "bed" "moon" "steal") 
     :diseases '("ear pain" "heart pain" "blood" "urinary tract" "bladder" "kidney deficiency diarrhea" "DM" 
		 "bleeding" "immune system" "STD" 
		 "spermachorrea" "genital" "toxic" "virus" "back" "heart" "fatique" "cold" "severe" "swollen"))
    (:name Li :yin-yang Yin :5elements Fire 
     :nature Fire :season Summer :direction S :animal MaleChicken :family "Middle Daughter" :body Eye 
     :tendency '(beautiful rely attach) :weather Sunny :organ '(H SI) :ZhiQi XiaZhi :color '()
     :others '("sun" "electricity" "helmet" "war" "big tommy" "bei jia" "crab" "skinny" "clam" "fish")
     :diseases '("eye" "heart" "halusination" "burn" "sun stroke" "radiation" "breast" "inflammation" "heat" 
		 "fever" "yellow urine" "blood" 
		 "spread" "enlargement" "lady problem"))
    (:name Gen :yin-yang Yang :5elements Earth 
     :nature Mountain :season Winter-Spring :direction NE :animal Dog :family "Youngest Son" :body Hand 
     :tendency '(stop) :weather Foggy :organ '(ST) :ZhiQi Spring :color '()
     :others '("small road" "small stone" "melon" "temple" "finger" "mice")
     :diseases '("Stomach" "no appetite" "distension" "nose" "hand" "leg" "back" "numbness" "joint" 
		 "blood stasis" "blister" "skin" "hard swollen"
		 "swollen upward inflammation" "wierd" "malnutritient" "tumor" "cancer" "stone" "poor circulation"))
    (:name Dui :yin-yang Yin :5elements Metal 
     :nature Swam :season Autumn :direction W :animal Sheep :family "Yongest Daughter" :body Mouth 
     :tendency '(joy break) :weather Rain :organ LU :ZhiQi ChauFan :color '()
     :others '("fortune teller" "tongue" "drop fruit" "dry land")
     :diseases '("mouth" "teeth" "tongue" "throat" "cough" "dyspnea" "chest distension" "Lung" "poor appetite" 
		 "Bladder" "urinary tract openning" "anus" "STD" "low blood pressure" "shortness of breath" 
		 "anemia" "external injury" "mild" "skin" "head injury" "bronchial"))
))

