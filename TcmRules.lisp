;;; Define the rules for expert system using lisa
;;; to do TCM diagnosis and treatment particularly base only on the
;;; reliable sources of Classics, like Shan Hang Lun or Yi Zhong Jin Gan
;;;

(in-package :lisa-user)

(make-inference-engine)

(deftemplate Tongue ()
	(slot body-color)
	(slot shape)
	(slot teethmark)
	(slot coating-color)
	(slot dry-or-moist)
	(slot red-spots)
	(slot purple-spots)
	(slot dark-vien))

(deftemplate Pulse ()
	(slot kind)
	(slot rate)
	(slot rhythm)
	(slot left-cun)
	(slot left-kun)
	(slot left-chi)
	(slot right-cun)
	(slot right-kun)
	(slot right-chi))

(deftemplate Indication ()
  	(slot fever)
  	(slot temp-feeling)	; hot, cold or alternate
	(slot aversion)			; cold or wind or others
	(slot back-aversion-cold)
	(slot appetite)
  	(slot thirst)
  	(slot pain)					; location
  	(slot headache)			; location
  	(slot neck-pain)
	(slot energy)
	(slot sleepy)
	(slot sweat)
	(slot sore-throat)
	(slot duration))

(deftemplate Diagnosis ()
  (slot disease)
  (slot diagnosis))

(deftemplate Treatment ()
	(slot acupuncture)
	(slot formula)
	(slot food-cure)
	(slot others))

(deftemplate Partial ()
	(slot result))

(defrule head-or-neck-pain ()
	(Indication (neck-pain yes))
	=>
	(assert (Partial (result head-or-neck-pain))))

(defrule head-or-neck-pain1 ()
	(Indication (headache yes))
	=>
	(assert (Partial (result head-or-neck-pain))))

(defrule SHL1 ()
	(Pulse (kind floating))
	(Partial (result head-or-neck-pain))
	(Indication (aversion cold))
	=>
	(format t "Tai Yang Bing - Sheng Hang Lun[1]")
	(assert (Diagnosis (disease Tai-Yang-Bing))))

(defrule SHL2 ()
	(Diagnosis (disease Tai-Yang-Bing))
	(Pulse (kind slow))		; pulse need to be modified into a list of kinds
	(Indication (sweat yes))
	(Indication (aversion wind))
	=>
	(format t "Zhong Hang - Sheng Hang Lun[2]")
	(assert (Diagnosis (diagnosis Zhong-Fang))))

(defrule SHL3 ()
	(Diagnosis (disease Tai-Yang-Bing))
	(Partial (result fever-or-not))
	(Indication (aversion cold))
	(Indication (pain body))
	(Indication (vomit-or-nausea yes))
	(Pulse (kind thight))
	=>
	(format t "Shang Hang - Sheng Hang Lun[3]")
	(assert (Diagnosis (diagnosis Shang-Hang))))

(defrule SHL4-a ()
	(Diagnosis (disease Tai-Yang-Bing))
    (Indication (duration 1day))
	(Pulse (kind not-rapid))
	=>
	(format t "Still Tai-Yang-Bing - Sheng Hang Lun[4]"))

(defrule SHL4-b ()
	(?dx (Diagnosis (disease Tai-Yang-Bing)))
    (Indication (duration 1day))
    (Indication (irritable yes))
	(Pulse (kind rapid))
	=>
	(format t "No longer Tai-Yang-Bing - Sheng Hang Lun[4]")
	(retract ?dx))

(defrule SHL5 ()
	(Diagnosis (disease Tai-Yang-Bing))
    (Indication (duration >=2 <=3 day))
	(not (Diagnosis (disease Yang-Ming-Bing)))
	(not (Diagnosis (disease Shao-Yang-Bing)))
	=>
	(format t "Still Tai-Yang-Bing - Sheng Hang Lun[5]"))

(defrule SHL6 ()
	(Diagnosis (disease Tai-Yang-Bing))
	(Indication (fever yes))
	(Indication (thrist yes))
	(not (Indication (aversion cold)))
	=>
	(format t "Not Tai-Yang-Bing. It's heat febrile disease. - Sheng Hang Lun[6]")
	(assert (Diagnosis (disease Febrile))))
	


(defrule Gui-Zhi-Tang ()
	(Diagnosis (disease Tai-Yang-Bing))
	(Indication (sweat yes))
	=>
	(format t "Use Gui Zhi Tang")
	(assert (Treatment (formula "Gui Zhi Tang"))))


(defrule Aversion-Cold1a ()
	 (Indication (aversion cold))
	 (Indication (fever yes))
	 (Indication (sweat yes))
	 =>
	(format t "Aversion to cold with fever and sweating, use Gui Zhi Tang. - YZGG[Ch37]")
	(assert (Diagnosis (disease superficial-def)))
	(assert (Treatment (formula "Gui Zhi Tang"))))

(defrule Aversion-Cold1b ()
	 (Indication (aversion cold))
	 (Indication (fever yes))
	 (Indication (sweat no))
	 =>
	(format t "Aversion to cold with fever and no sweating, use Ma Huang Tang. - YZGG[Ch37]")
	(assert (Diagnosis (disease superficial-excess)))
	(assert (Treatment (formula "Ma Huang Tang"))))

(defrule Aversion-Cold2a ()
	 (Indication (aversion cold))
	 (Indication (fever no))
	 (Indication (sweat yes))
	 =>
	(format t "Aversion to cold with no fever and no sweating, use Gui Zhi Jia Fu Zi Tang. - YZGG[Ch37]")
	(assert (Diagnosis (disease internal)))
	(assert (Treatment (formula "Gui Zhi Jia Fu Zi Tang"))))
 
(defrule Aversion-Cold2b ()
	 (Indication (aversion cold))
	 (Indication (fever no))
	 (Indication (sweat no))
	 =>
	(format t "Aversion to cold with no fever but sweating, use Ma Huang Xi Xin Fu Zi Tang. - YZGG[Ch37]")
	(assert (Diagnosis (disease internal)))
	(assert (Treatment (formula "Ma Huang Xi Xin Fu Zi Tang"))))
 
(defrule Aversion-Cold3a ()
	 (Indication (aversion cold))
	 (Indication (fever no))
	 (Indication (back-averson-cold yes))
	 (Indication (appetite normal))
	 =>
	(format t "Aversion to cold especially back with no fever and normal appetite, use Fu Zi Tang. - YZGG[Ch37]")
	(assert (Diagnosis (disease Shao-Yin)))
	(assert (Treatment (formula "Fu Zi Tang"))))

(defrule Aversion-Cold3b ()
	 (Indication (aversion cold))
	 (Indication (fever no))
	 (Indication (back-averson-cold yes))
	 (Indication (dry-mouth yes))
	 (Indication (thirst yes))
	 =>
	(format t "Aversion to cold especially back with no fever but dry mouth and thirst, use Bai Hu Jia Ren Shen Tang. - YZGG[Ch37]")
	(assert (Diagnosis (disease Yang-Ming)))
	(assert (Treatment (formula "Bai Hu Jia Ren Shen Tang"))))
;;;
;;;
;;;
;;; To inspect some facts
;;; (retrieve (?x ?disease) (?x (diagnosis (disease ?disease))))
