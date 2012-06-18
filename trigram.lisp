(defvar *5elements-db* nil)
(setf *5elements-db* '(
		       (:name "Wood" :property '("life" "excite"))
		       (:name "Fire" :property '("change" "movement"))
		       (:name "Earth" :property '("give birth" "nurture"))
		       (:name "Metal" :property '("regulate" "rule"))
		       (:name "Water" :property '("hidden" "end"))))

(defvar *Trigram* '(Qian Kun Zhen Xun Kan Li Gen Dui))
(defvar *Trigram-db* 
  '((:name Qian 
     :yin-yang Yang 
     :5elements Metal 
     :nature Sky 
     :season Autumn-Winter 
     :direction NW 
     :animal Horse 
     :family "Father" 
     :body Head 
     :tendency '(Strong-Move) 
     :weather Sunny 
     :organ '(LI) 
     :ZhiQi NaDong 
     :color '(red) 
     :others '("nation" "king" "jade" "gold" "cold" "ice" "wood-fruit")
     :indications '("head" "chest" "bone pain" "harden" "chronic" "wind-cold" 
		    "abnormal change" "acute" "blockage" "constipation" "nerve" "stroke"))
    (:name Kun 
     :yin-yang Yin 
     :5elements Earth 
     :nature Earth 
     :season Summer-Autumn 
     :direction SW 
     :animal Cow 
     :family "Mother" 
     :body Abdomen 
     :tendency '(Follow Gental Stingy) 
     :weather Cloud 
     :organ '(SP) 
     :ZhiQi NaChao 
     :color '(black-soil) 
     :others '("cloth" "axe" "car" "text" "crowd" "handle")
     :indications '("abdomen" "GI" "digestion" "swollen" "dampness" "skin" "blister" 
		    "faint" "fatique" "chronic" "middle Qi deficient" "cancer"
		    "muscle weakness" "distension" "serious severe disease" "dieing"))
    (:name Zhen 
     :yin-yang Yang 
     :5elements Wood 
     :nature Thunder 
     :season Spring 
     :direction E 
     :animal Dragon 
     :family "Eldest Son" 
     :body Leg 
     :tendency '(Start-Move Give Decisive) 
     :weather Thunder 
     :organ '(LIV) 
     :ZhiQi ChungFan 
     :color '(black-yellow) 
     :others '("big road" "young bamboo" "root-fruit" "pennut" "yam" "potato")
     :indications '("emotion" "mental" "manic" "sensitive" "epilapsy" "nervious" "hyeractive" 
		    "lady problem" "liver fire" "pain" "swollen leg" "tramma" "acute" 
		    "servere" "cough" "vocal" "throat" "liver" "fatty liver" "hepititis"))
    (:name Xun 
     :yin-yang Yin 
     :5elements Wood 
     :nature Wind 
     :season Spring-Summer 
     :direction SE 
     :animal Chicken 
     :family "Eldest Daughter" 
     :body Buttock 
     :tendency '(inward) 
     :weather Wind 
     :organ '(GB) 
     :ZhiQi NaXia 
     :color '(white)
     :others '("vertial rob" "work" "long" "high" "advance" "drawback" "no fruit" "odor" 
	       "bold" "white eye")
     :indications '("wind" "stroke" "emotion" "mental" "gall bladder" "infection" "sciatia" 
		    "lymph" 
		    "cramp" "stiff" "unstable" "dyspnea" "asthema" "left shoulder pain"
		    "nerve inflammation" "hip pain" "chest distention" "drunk" 
		    "abdomen distention" "depression" 
		    "blood vessel" "gall stone" "gall bladder infection" "bile duct"))
    (:name Kan 
     :yin-yang Yang 
     :5elements Water 
     :nature Water 
     :season Winter 
     :direction N 
     :animal Pig 
     :family "Middle Son" 
     :body Ear 
     :tendency '(descend danger hide gental-beautiful worry) 
     :weather Rain 
     :organ '(K BL) 
     :ZhiQi DongZhi 
     :color '()
     :others '("sewage" "car" "bed" "moon" "steal") 
     :indications '("ear pain" "heart pain" "blood" "urinary tract" "bladder" 
		    "kidney deficiency diarrhea" "DM" 
		    "bleeding" "immune system" "STD" 
		    "spermachorrea" "genital" "toxic" "virus" "back" "heart" "fatique" 
		    "cold" "severe" "swollen"))
    (:name Li 
     :yin-yang Yin 
     :5elements Fire 
     :nature Fire 
     :season Summer 
     :direction S 
     :animal MaleChicken 
     :family "Middle Daughter" 
     :body Eye 
     :tendency '(beautiful rely attach) 
     :weather Sunny 
     :organ '(H SI) 
     :ZhiQi XiaZhi 
     :color '()
     :others '("sun" "electricity" "helmet" "war" "big tommy" "bei jia" "crab" "skinny" 
	       "clam" "fish")
     :indications '("eye" "heart" "halusination" "burn" "sun stroke" "radiation" "breast" 
		    "inflammation" "heat" "fever" "yellow urine" "blood" 
		    "spread" "enlargement" "lady problem"))
    (:name Gen 
     :yin-yang Yang 
     :5elements Earth 
     :nature Mountain 
     :season Winter-Spring 
     :direction NE 
     :animal Dog 
     :family "Youngest Son" 
     :body Hand 
     :tendency '(stop) 
     :weather Foggy 
     :organ '(ST) 
     :ZhiQi Spring 
     :color '()
     :others '("small road" "small stone" "melon" "temple" "finger" "mice")
     :indications '("Stomach" "no appetite" "distension" "nose" "hand" "leg" "back" 
		    "numbness" "joint" 
		    "blood stasis" "blister" "skin" "hard swollen"
		    "swollen upward inflammation" "wierd" "malnutritient" "tumor" "cancer" 
		    "stone" "poor circulation"))
    (:name Dui 
     :yin-yang Yin 
     :5elements Metal 
     :nature Swam 
     :season Autumn 
     :direction W 
     :animal Sheep 
     :family "Yongest Daughter" 
     :body Mouth 
     :tendency '(joy break) 
     :weather Rain 
     :organ LU 
     :ZhiQi ChauFan 
     :color '()
     :others '("fortune teller" "tongue" "drop fruit" "dry land")
     :indications '("mouth" "teeth" "tongue" "throat" "cough" "dyspnea" "chest distension" 
		    "Lung" "poor appetite" 
		    "Bladder" "urinary tract openning" "anus" "STD" "low blood pressure" 
		    "shortness of breath" 
		    "anemia" "external injury" "mild" "skin" "head injury" "bronchial"))
))

; example use
(selectf *Trigram-db* (indications-selector "blood") :organ)
