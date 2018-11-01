(clear) 
(deffunction  ask   (?question  $?allowed) 
  (printout  t  ?question  ?allowed)  
  (bind  ?answer  (read)) 
?answer 
) 

(deffunction  ask-allowed   (?question $?allowed)  
        (bind   ?answer (ask ?question)) 
  (while (not (member  ?answer $?allowed) )  
   do  
    (printout t "Reenter, please" crlf) 
    (bind ?answer (ask ?question)) 
 ) 
?answer 
) 

(deffunction ask-yes-no   (?question) 
  (bind  ?response   (ask-allowed  ?question yes no))  
   (eq ?response yes) 
) 
(deftemplate symptom (slot name) (multislot type) (slot question) (slot answer))
(deffacts example 
	(symptom (name skin_pigmentation) (type pneumonia) (question "Do you have pigmentation in skin?  1)-YES  2)-NO  3)-REASON  ") (answer unknown))
	(symptom (name pain_in_chest) (type pneumonia) (question "Do you have pain in chest?  1)-YES  2)-NO  3)-REASON  ") (answer unknown))

	(symptom (name dark_urine) (type jaundice) (question "Is your urine dark in colour?  1)-YES  2)-NO  3)-REASON  ") (answer unknown))

	(symptom (name diarrhoea) (type cholera) (question "Do you have diarrhoea?  1)-YES  2)-NO  3)-REASON  ") (answer unknown))
    (symptom (name shivering) (type pneumonia malaria) (question "Are you shivering?  1)-YES  2)-NO  3)-REASON  ") (answer unknown))
    (symptom (name hallucinations) (type encephalitis) (question "Are there hallucinations?  1)-YES  2)-NO  3)-REASON  ") (answer unknown))
	(symptom (name temperature) (type flu malaria pneumonia encephalitis pharingitis poisoning measles meningitis) (question "Is there temperature?  1)-YES  2)-NO  3)-REASON  ") (answer unknown))
	(symptom (name pain_in_muscles) (type encephalitis malaria flu poisoning meningitis) (question "Is there pain in muscles?  1)-YES  2)-NO  3)-REASON ") (answer unknown))
	(symptom (name cought) (type flu ORVI laringitis pharingitis bronchitis measles meningitis) (question "Is there cought?  1)-YES  2)-NO  3)-REASON ") (answer unknown))
	(symptom (name weaknesses) (type pneumonia jaundice encephalitis cholera flu malaria ORVI migraine bronchitis measles meningitis anemia) (question "Is there weaknesses?  1)-YES  2)-NO  3)-REASON ") (answer unknown))
	(symptom (name vomit) (type flu jaundice gastritis cholera migraine poisoning meningitis) (question "Is there vomit?  1)-YES  2)-NO  3)-REASON ") (answer unknown))
	(symptom (name pain_in_stomach) (type jaundice cholera gastritis) (question "Is there pain in stomach?  1)-YES  2)-NO  3)-REASON ") (answer unknown))
	(symptom (name running_nose) (type flu ORVI measles) (question "Is there running nose?  1)-YES  2)-NO  3)-REASON ") (answer unknown))
	(symptom (name sore_throat) (type ORVI laringitis pharingitis bronchitis) (question "Is there sore throat?  1)-YES  2)-NO  3)-REASON ") (answer unknown))
	(symptom (name hoarseness) (type laringitis) (question "Is there hoarseness?  1)-YES  2)-NO  3)-REASON ") (answer unknown))
	(symptom (name headache) (type migraine rubella bronchitis measles meningitis anemia encephalitis) (question "Is there headache?  1)-YES  2)-NO  3)-REASON ") (answer unknown))
	(symptom (name red_eyes) (type rubella bronchitis) (question "Is there red eyes?  1)-YES  2)-NO  3)-REASON ") (answer unknown))
	(symptom (name rash) (type rubella measles meningitis) (question "Is there rash?  1)-YES  2)-NO  3)-REASON ") (answer unknown))
	(symptom (name faint) (type anemia encephalitis) (question "Is there faint?  1)-YES  2)-NO  3)-REASON ") (answer unknown))
	(symptom (name paleness) (type anemia pneumonia) (question "Is there paleness?  1)-YES  2)-NO  3)-REASON ") (answer unknown))
)


;;------------------------------------------------------------------------------------------------------------------------------
(defrule ask_pneumonia
(declare (salience 170))
(not (suggest ?))
(not(no $? pneumonia $?))
?f1 <- (symptom(name ?name) (type $?x pneumonia $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then 
(modify ?f1 (answer yes))
(assert( name ?name ))
else (if (eq ?response 2)
then
(modify ?f1 (answer no))
(assert(no $?x pneumonia $?z))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have pneumonia" crlf))
)
)
)

(defrule pneumonia
(declare (salience 165))
(not(suggest ?))
(name temperature)
(name shivering)
(name weaknesses)
(name pain_in_chest)
(name paleness)
(name skin_pigmentation)
=>
(assert(suggest "May be this is pneumonia, because you have fever, chills, pain in chest, paleness and skin pigmentation"))
)

;;------------------------------------------------------------------------------------------------------------------------------
(defrule ask_jaundice
(declare (salience 160))
(not (suggest ?))
(not(no $? jaundice $?))
?f1 <- (symptom(name ?name) (type $?x jaundice $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then 
(modify ?f1 (answer yes))
(assert( name ?name ))
else (if (eq ?response 2)
then
(modify ?f1 (answer no))
(assert(no $?x jaundice $?z))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have jaundice" crlf))
)
)
)

(defrule jaundice
(declare (salience 155))
(not(suggest ?))
(name dark_urine)
(name vomit)
(name weaknesses)
(name pain_in_stomach)
=>
(assert(suggest "May be this is jaundice, because you have dark_urine, vomit, pain in stomach and weaknesses"))
)

;;------------------------------------------------------------------------------------------------------------------------------
(defrule ask_cholera
(declare (salience 150))
(not (suggest ?))
(not(no $? cholera $?))
?f1 <- (symptom(name ?name) (type $?x cholera $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then 
(modify ?f1 (answer yes))
(assert( name ?name ))
else (if (eq ?response 2)
then
(modify ?f1 (answer no))
(assert(no $?x cholera $?z))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have cholera" crlf))
)
)
)

(defrule cholera
(declare (salience 145))
(not(suggest ?))
(name diarrhoea)
(name vomit)
(name weaknesses)
(name pain_in_stomach)
=>
(assert(suggest "May be this is cholera, because you have vomit, diarrhoea, pain in stomach and weaknesses"))
)


;;------------------------------------------------------------------------------------------------------------------------------
(defrule ask_malaria
(declare (salience 140))
(not (suggest ?))
(not(no $? malaria $?))
?f1 <- (symptom(name ?name) (type $?x malaria $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then 
(modify ?f1 (answer yes))
(assert( name ?name ))
else (if (eq ?response 2)
then
(modify ?f1 (answer no))
(assert(no $?x malaria $?z))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have a malaria" crlf))
)
)
)

(defrule malaria
(declare (salience 135))
(not(suggest ?))
(name temperature)
(name shivering)
(name weaknesses)
(name pain_in_muscles)
=>
(assert(suggest "May be this is malaria, because you have temperature, shivering, pain in muscles and weaknesses"))
)

;;------------------------------------------------------------------------------------------------------------------------------
(defrule ask_encephalitis
(declare (salience 130))
(not (suggest ?))
(not(no $? encephalitis $?))
?f1 <- (symptom(name ?name) (type $?x encephalitis $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then 
(modify ?f1 (answer yes))
(assert( name ?name ))
else (if (eq ?response 2)
then
(modify ?f1 (answer no))
(assert(no $?x encephalitis $?z))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have a encephalitis" crlf))
)
)
)

(defrule encephalitis
(declare (salience 125))
(not(suggest ?))
(name temperature)
(name headache)
(name faint)
(name weaknesses)
(name pain_in_muscles)
(name hallucinations)
=>
(assert(suggest "May be this is encephalitis, because you have temperature, headache, faint, weakness and pain in muscles"))
)

;;------------------------------------------------------------------------------------------------------------------------------
(defrule ask_meningitis 
(declare (salience 120))
(not (suggest ?))
(not(no $? meningitis $?))
?f1 <- (symptom(name ?name) (type $?x meningitis $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then 
(modify ?f1 (answer yes))
(assert( name ?name ))
else (if (eq ?response 2)
then
(modify ?f1 (answer no))
(assert(no $?x meningitis $?z))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have a meningitis" crlf))
)
)
)

(defrule meningitis
(declare (salience 115))
(not(suggest ?))
(name temperature)
(name headache)
(name weaknesses)
(name vomit)
(name cought)
(name rash)
(name pain_in_muscles)
=>
(assert(suggest "May be this is meningitis, because you have temperature, headache, cought, pain in muscles, weaknesses, rash and vomit"))
)

;;------------------------------------------------------------------------------------------------------------------------------
(defrule ask_anemia 
(declare (salience 110))
(not (suggest ?))
(not(no $? anemia $?))
?f1 <- (symptom(name ?name) (type $?x anemia $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
	(if (eq ?response 1)
	then 
	(modify ?f1 (answer yes))
	(assert( name ?name ))
	else (if (eq ?response 2)
	then
	(modify ?f1 (answer no))
	(assert(no $?x anemia $?z))
	else 
	(if (eq ?response 3)
	then
	(modify ?f1 (answer unknown))
	(printout t "There is suggest that you have anemia" crlf))
	)
	)
)

(defrule anemia
(declare (salience 105))
(not(suggest ?))
(name headache)
(name weaknesses)
(name paleness)
(name faint)
=>
(assert(suggest "May be this is anemia, because you have headache, weaknesses, paleness and faint"))
)


;;------------------------------------------------------------------------------------------------------------------------------
(defrule ask_measles 
(declare (salience 100))
(not (suggest ?))
(not(no $? measles $?))
?f1 <- (symptom(name ?name) (type $?x measles $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then 
(modify ?f1 (answer yes))
(assert( name ?name ))
else (if (eq ?response 2)
then
(modify ?f1 (answer no))
(assert(no $?x measles $?z))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have a measles" crlf))
)
)
)

(defrule measles
(declare (salience 95))
(not(suggest ?))
(name temperature)
(name headache)
(name weaknesses)
(name running_nose)
(name cought)
(name rash)
=>
(assert(suggest "May be this is measles, because you have temperature, headache, cought, running nose, weaknesses and rash"))
)
;;------------------------------------------------------------------------------------------------------------------------------
(defrule ask_bronchitis 
(declare (salience 90))
(not (suggest ?))
(not(no $? bronchitis $?))
?f1 <- (symptom(name ?name) (type $?x bronchitis $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then 
(modify ?f1 (answer yes))
(assert( name ?name ))
else (if (eq ?response 2)
then
(modify ?f1 (answer no))
(assert(no $?x bronchitis $?z))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have a bronchitis" crlf))
)
)
)

(defrule bronchitis
(declare (salience 85))
(not(suggest ?))
(name red_eyes)
(name headache)
(name sore_throat)
(name cought)
(name weaknesses)
=>
(assert(suggest "May be this is poisoning, because you have red eyes, headache, sore throat, cought and weaknesses"))
)
;;------------------------------------------------------------------------------------------------------------------------------
(defrule ask_poisoning 
(declare (salience 80))
(not (suggest ?))
(not(no $? poisoning $?))
?f1 <- (symptom(name ?name) (type $?x poisoning $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then 
(modify ?f1 (answer yes))
(assert( name ?name ))
else (if (eq ?response 2)
then
(modify ?f1 (answer no))
(assert(no $?x poisoning $?z))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have a poisoning" crlf))
)
)
)

(defrule poisoning 
(declare (salience 75))
(not(suggest ?))
(name temperature)
(name pain_in_muscles)
(name vomit)
=>
(assert(suggest "May be this is poisoning, because you have temperature, pain in muscles and vomit"))
)

;;------------------------------------------------------------------------------------------------------------------------------
(defrule ask_rubella 
(declare (salience 70))
(not (suggest ?))
(not(no $? rubella $?))
?f1 <- (symptom(name ?name) (type $?x rubella $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then 
(modify ?f1 (answer yes))
(assert( name ?name ))
else (if (eq ?response 2)
then
(modify ?f1 (answer no))
(assert(no $?x rubella $?z))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have a rubella" crlf))
)
)
)

(defrule rubella 
(declare (salience 65))
(not(suggest ?))
(name headache)
(name red_eyes)
(name rash)
=>
(assert(suggest "May be this is rubella, because you have headache, red eyes and rash"))
)
;;------------------------------------------------------------------------------------------------------------------------------
(defrule ask_flu 
(declare (salience 60))
(not (suggest ?))
(not(no $? flu $?))
?f1 <- (symptom(name ?name) (type $?x flu $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then 
(modify ?f1 (answer yes))
(assert( name ?name ))
else (if (eq ?response 2)
then
(modify ?f1 (answer no))
(assert(no $?x flu $?z))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have a flu" crlf))
)
)
)

(defrule flu 
(declare (salience 55))
(not(suggest ?))
(name temperature)
(name pain_in_muscles)
(name cought)
(name weaknesses)
(name vomit)
(name running_nose)
=>
(assert(suggest "May be this is flu"))
)

;;------------------------------------------------------------------------------------------------------------------------------

(defrule ask_gastritis
(declare (salience 50))
(not (suggest ?))
(not(no $? gastritis $?))
?f1 <- (symptom(name ?name) (type $?x gastritis $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then  
(modify ?f1 (answer yes))
(assert( name ?name ))
else (if (eq ?response 2)
then
(modify ?f1 (answer no))
(assert(no $?x gastritis $?z))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have a problem with your stomack" crlf))
)
)
)

(defrule gastritis 
(declare (salience 45))
(not(suggest ?))
(name vomit)
(name pain_in_stomach)
=>
(assert(suggest "May be this is gastritis"))
)

;;------------------------------------------------------------------------------------------------------------------------------

(defrule ask_ORVI 
(declare (salience 40))
(not (suggest ?))
(not(no $? ORVI $?))
?f1 <- (symptom(name ?name) (type $?x ORVI $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then 
(modify ?f1 (answer yes))
(assert( name ?name ))
else (if (eq ?response 2)
then
(modify ?f1 (answer no))
(assert(no $?x ORVI $?z))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have a ORVI" crlf))
)
)
)

(defrule ORVI 
(declare (salience 35))
(not(suggest ?))
(name cought)
(name weaknesses)
(name running_nose)
(name sore_throat)
=>
(assert(suggest "May be this is ORVI"))
)

;;------------------------------------------------------------------------------------------------------------------------------

(defrule ask_laringitis
(declare (salience 30))
(not (suggest ?))
(not(no $? laringitis $?))
?f1 <- (symptom(name ?name) (type $?x laringitis $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then 
(modify ?f1 (answer yes))
(assert( name ?name ))
else (if (eq ?response 2)
then
(modify ?f1 (answer no))
(assert(no $?x laringitis $?z))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have a laringitis" crlf))
)
)
)

(defrule laringitis 
(declare (salience 25))
(not(suggest ?))
(name cought)
(name sore_throat)
(name hoarseness) 
=>
(assert(suggest "May be this is laringitis"))
)

;;------------------------------------------------------------------------------------------------------------------------------

(defrule ask_pharyngitis
(declare (salience 20))
(not (suggest ?))
(not(no $? pharingitis $?))
?f1 <- (symptom(name ?name) (type $?x pharingitis $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then 
(modify ?f1 (answer yes))
(assert( name ?name ))
else (if (eq ?response 2)
then
(modify ?f1 (answer no))
(assert(no $?x pharingitis $?z))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have a pharingitis" crlf))
)
)
)

(defrule pharingitis
(declare (salience 15))
(not(suggest ?))
(name temperature)
(name cought)
(name sore_throat)
=>
(assert(suggest "May be this is pharingitis"))
)


;;------------------------------------------------------------------------------------------------------------------------------

(defrule ask_migraine
(declare (salience 10))
(not (suggest ?))
(not(no $? migraine $?))
?f1 <- (symptom(name ?name) (type $?x migraine $?z) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then 
(modify ?f1 (answer yes))
(assert( name ?name ))
else (if (eq ?response 2)
then
(modify ?f1 (answer no))
(assert(no $?x migraine $?z))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have a migraine" crlf))
)
)
)

(defrule migraine
(declare (salience 5))
(not(suggest ?))
(name headache)
(name vomit)
(name weaknesses)
=>
(assert(suggest "May be this is migraine, because you have headache, vomit and weaknesses"))
)
;;------------------------------------------------------------------------------------------------------------------------------

(defrule PrintSuggest   
 (declare (salience 1000))
 (suggest ?x) 
 => 
  (printout t ?x crlf) 
) 
 
(defrule NoSuggest    
  (declare (salience -10)) 
  (not (suggest ?)) 
 => 
  (printout t "Sorry, there is no suggest." crlf) 
)