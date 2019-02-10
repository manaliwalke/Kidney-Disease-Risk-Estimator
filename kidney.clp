(deftemplate personBio
	(slot age)
	(slot bmidiabp)
	(slot smokedrink)
	(slot fam)
)

(deftemplate validInput
	(slot validity)
)


(defglobal
	?*name* = "yo"
	?*age* = 2
	?*height* = 2
	?*weight* = 2
	?*bmi* = 2
	?*bmival* = 2
	?*diabetes* = 2
	?*bp* = 2
	?*smoke* = 2
	?*drink* = 2
	?*fam* = 2
	?*validity* = 1
)

(defrule takeInput
    =>
    (printout t crlf crlf "****WELCOME TO THE KIDNEY DISEASE RISK ESTIMATOR****" crlf)
    (printout t "$$ This Application provides an estimate on how much risk prone you are to kidney disease and what you can try to improve" crlf)
    (printout t "PLEASE PROVIDE A VALID INPUT FOR ALL THE QUESTIONS" crlf)
    (printout t "--------------------Patient Information-----------------------" crlf crlf)        
    (printout t "Please enter your name: " crlf)
    (bind ?*name* (read t))
    (printout t "What is your age?(Ranging from 1 to 100)" crlf)
    (bind ?*age* (read t))
    (printout t "What is your height?(cm)(Ranging from 1 to 250)" crlf)
    (bind ?*height* (read t))
    (printout t "What is your weight?(kg)(Ranging from 1 to 200)" crlf)
    (bind ?*weight* (read t))
    (printout t "Do you have diabetes?[y/n]" crlf)
    (bind ?*diabetes* (read t))
    (printout t "Do you have high BP?[y/n]" crlf)
    (bind ?*bp* (read t))
    (printout t "Do you smoke?[y/n]" crlf)
    (bind ?*smoke* (read t))
    (printout t "Do you drink?[y/n]" crlf)
    (bind ?*drink* (read t))
    (printout t "Do you have a family history of kidney disease?[y/n]" crlf)
    (bind ?*fam* (read t))
    (inputCheck)      
)

(deffunction inputCheck()
	(bind ?*validity* 0)
	(if (= (str-compare ?*diabetes* "y") 0) then
        	(bind ?*diabetes* 1)
        else
        (if (= (str-compare ?*diabetes* "n") 0) then
        		(bind ?*diabetes* 0)
        	else
        		(bind ?*validity* 1)
        )
    )

    (if (= (str-compare ?*bp* "y") 0) then
        	(bind ?*bp* 1)
        else
        (if (= (str-compare ?*bp* "n") 0) then
        		(bind ?*bp* 0)
        	else
        		(bind ?*validity* 1)
        )
    )

    (if (= (str-compare ?*smoke* "y") 0) then
        	(bind ?*smoke* 1)
        else
        (if (= (str-compare ?*smoke* "n") 0) then
        		(bind ?*smoke* 0)
        	else
        		(bind ?*validity* 1)
        )
    )

    (if (= (str-compare ?*drink* "y") 0) then
        	(bind ?*drink* 1)
        else
        (if (= (str-compare ?*drink* "n") 0) then
        		(bind ?*drink* 0)
        	else
        		(bind ?*validity* 1)
        )
    )

    (if (= (str-compare ?*fam* "y") 0) then
        	(bind ?*fam* 1)
        else
        (if (= (str-compare ?*fam* "n") 0) then
        		(bind ?*fam* 0)
        	else
        		(bind ?*validity* 1)
        )
    )

    (if (and (<= 1 ?*age*)(<= ?*age* 40)) then
        	(bind ?*age* 0)
        else
        (if (and (<= 41 ?*age*)(<= ?*age* 100)) then
        		(bind ?*age* 1)
        	else
        		(bind ?*validity* 1)
        )
    )

    (if (or (<= ?*height* 1)(>= ?*height* 250)) then
        	(bind ?*validity* 1)

    )

    (if (or (>= 1 ?*weight*)(>= ?*weight* 200)) then
        	(bind ?*validity* 1)
    )


    (bmiCalculator)
    (if (and (<= 10 ?*bmi*)(<= ?*bmi* 25)) then
        	(bind ?*bmi* 0)
        else
        (if (< 25 ?*bmi*) then
        		(bind ?*bmi* 1)
        	else
        		(bind ?*validity* 1)
        )
    )

    (if (and (= ?*smoke* 0)(= ?*drink* 0)) then
        	(bind ?*smokedrink* 0)
        else
        	(bind ?*smokedrink* 1)
        
    )

    (if (and (and (= ?*bmi* 0)(= ?*diabetes* 0)) (= ?*bp* 0)) then
        	(bind ?*bmidiabp* 0)
        else
        	(bind ?*bmidiabp* 1)
        
    )    

    (assert (validInput (validity ?*validity*)))
    (assert (personBio (age ?*age*)(bmidiabp ?*bmidiabp*)(smokedrink ?*smokedrink*)(fam ?*fam*)))
)

(deffunction bmiCalculator()
	(bind ?*height* (/ ?*height* 100))
	(bind ?*bmi* (/ ?*weight* (* ?*height* ?*height*)))
	(bind ?*bmival* ?*bmi*)
)

(defrule rule90
	(validInput (validity 0))(personBio (age 1)(bmidiabp 1)(smokedrink 1)(fam 1))
	=>
	(printout t "--------------Patient Report--------------" crlf crlf "Hello " ?*name* "!" crlf crlf "Chances of kidney disease are 90%." crlf crlf "Body Mass Index(BMI) is " ?*bmival* crlf crlf "Please visit a doctor urgently" crlf crlf "Improve your diet,exercise more and decrease smoking/drinking to lower the chances of having a kidney disease" crlf crlf)
)

(defrule rule85
	(validInput (validity 0))(personBio (age 1)(bmidiabp 1)(smokedrink 1)(fam 0))
	=>
	(printout t "--------------Patient Report--------------" crlf crlf "Hello " ?*name* "!" crlf crlf "Chances of kidney disease are 85%." crlf crlf "Body Mass Index(BMI) is " ?*bmival* crlf crlf "Please visit a doctor urgently" crlf crlf "Improve your diet,exercise more and decrease smoking/drinking to lower the chances of having a kidney disease" crlf crlf)
)

(defrule rule80
	(validInput (validity 0))(personBio (age 1)(bmidiabp 1)(smokedrink 0)(fam 1))
	=>
	(printout t "--------------Patient Report--------------" crlf crlf "Hello " ?*name* "!" crlf crlf "Chances of kidney disease are 80%." crlf crlf "Body Mass Index(BMI) is " ?*bmival* crlf crlf "Please visit a doctor urgently" crlf crlf "Improve your diet and exercise more to lower the chances of having a kidney disease in the future " crlf crlf)
)

(defrule rule75
	(validInput (validity 0))(personBio (age 1)(bmidiabp 1)(smokedrink 0)(fam 0))
	=>
	(printout t "--------------Patient Report--------------" crlf crlf "Hello " ?*name* "!" crlf crlf "Chances of kidney disease are 75%." crlf crlf "Body Mass Index(BMI) is " ?*bmival* crlf crlf "Improve your diet and exercise more to lower the chances of having a kidney disease in the future " crlf crlf)
)

(defrule rule70
	(validInput (validity 0))(personBio (age 1)(bmidiabp 0)(smokedrink 1)(fam 1))
	=>
	(printout t "--------------Patient Report--------------" crlf crlf "Hello " ?*name* "!" crlf crlf "Chances of kidney disease are 70%." crlf crlf "Body Mass Index(BMI) is " ?*bmival* crlf crlf "Try to decrease smoking/drinking to lower the chances of having a kidney disease in the future " crlf crlf)
)

(defrule rule65
	(validInput (validity 0))(personBio (age 1)(bmidiabp 0)(smokedrink 1)(fam 0))
	=>
	(printout t "--------------Patient Report--------------" crlf crlf "Hello " ?*name* "!" crlf crlf "Chances of kidney disease are 65%." crlf crlf "Body Mass Index(BMI) is " ?*bmival* crlf crlf "Try to decrease smoking/drinking to lower the chances of having a kidney disease in the future " crlf crlf)
)
(defrule rule60
	(validInput (validity 0))(personBio (age 1)(bmidiabp 0)(smokedrink 0)(fam 1))
	=>
	(printout t "--------------Patient Report--------------" crlf crlf "Hello " ?*name* "!" crlf crlf "Chances of kidney disease are 60%." crlf crlf "Body Mass Index(BMI) is " ?*bmival* crlf crlf "Have regular checkups to lower the chances of having a kidney disease in the future " crlf crlf)
)
(defrule rule55
	(validInput (validity 0))(personBio (age 1)(bmidiabp 0)(smokedrink 0)(fam 0))
	=>
	(printout t "--------------Patient Report--------------" crlf crlf "Hello " ?*name* "!" crlf crlf "Chances of kidney disease are 55%." crlf crlf "Body Mass Index(BMI) is " ?*bmival* crlf crlf "Have regular checkups to lower the chances of having a kidney disease in the future " crlf crlf)
)

(defrule rule50
	(validInput (validity 0))(personBio (age 0)(bmidiabp 1)(smokedrink 1)(fam 1))
	=>
	(printout t "--------------Patient Report--------------" crlf crlf "Hello " ?*name* "!" crlf crlf "Chances of kidney disease are 50%." crlf crlf "Body Mass Index(BMI) is " ?*bmival* crlf crlf "Improve your diet,exercise more and decrease smoking/drinking to lower the chances of having a kidney disease in the future " crlf crlf)
)

(defrule rule45
	(validInput (validity 0))(personBio (age 0)(bmidiabp 1)(smokedrink 1)(fam 0))
	=>
	(printout t "--------------Patient Report--------------" crlf crlf "Hello " ?*name* "!" crlf crlf "Chances of kidney disease are 45%." crlf crlf "Body Mass Index(BMI) is " ?*bmival* crlf crlf "Improve your diet,exercise more and decrease smoking/drinking to lower the chances of having a kidney disease in the future " crlf crlf)
)

(defrule rule40
	(validInput (validity 0))(personBio (age 0)(bmidiabp 1)(smokedrink 0)(fam 1))
	=>
	(printout t "--------------Patient Report--------------" crlf crlf "Hello " ?*name* "!" crlf crlf "Chances of kidney disease are 40%." crlf crlf "Body Mass Index(BMI) is " ?*bmival* crlf crlf "Improve your diet and exercise more to lower the chances of having a kidney disease in the future " crlf crlf)
)

(defrule rule35
	(validInput (validity 0))(personBio (age 0)(bmidiabp 1)(smokedrink 0)(fam 0))
	=>
	(printout t "--------------Patient Report--------------" crlf crlf "Hello " ?*name* "!" crlf crlf "Chances of kidney disease are 35%." crlf crlf "Body Mass Index(BMI) is " ?*bmival* crlf crlf "Improve your diet and exercise more to lower the chances of having a kidney disease in the future " crlf crlf)
)

(defrule rule30
	(validInput (validity 0))(personBio (age 0)(bmidiabp 0)(smokedrink 1)(fam 1))
	=>
	(printout t "--------------Patient Report--------------" crlf crlf "Hello " ?*name* "!" crlf crlf "Chances of kidney disease are 30%." crlf crlf "Body Mass Index(BMI) is " ?*bmival* crlf crlf "Try to decrease smoking/drinking to lower the chances of having a kidney disease in the future " crlf crlf)
)

(defrule rule25
	(validInput (validity 0))(personBio (age 0)(bmidiabp 0)(smokedrink 1)(fam 0))
	=>
	(printout t "--------------Patient Report--------------" crlf crlf "Hello " ?*name* "!" crlf crlf "Chances of kidney disease are 25%." crlf crlf "Body Mass Index(BMI) is " ?*bmival* crlf crlf "Try to decrease smoking/drinking to lower the chances of having a kidney disease in the future " crlf crlf)
)

(defrule rule20
	(validInput (validity 0))(personBio (age 0)(bmidiabp 0)(smokedrink 0)(fam 1))
	=>
	(printout t "--------------Patient Report--------------" crlf crlf "Hello " ?*name* "!" crlf crlf "Chances of kidney disease are 20%." crlf crlf "Body Mass Index(BMI) is " ?*bmival* crlf crlf "You are doing great!" crlf crlf)
)

(defrule rule15
	(validInput (validity 0))(personBio (age 0)(bmidiabp 0)(smokedrink 0)(fam 0))
	=>
	(printout t "--------------Patient Report--------------" crlf crlf "Hello " ?*name* "!" crlf crlf "Chances of kidney disease are 15%." crlf crlf "Body Mass Index(BMI) is " ?*bmival* crlf crlf "You are doing great!" crlf crlf)
)

(defrule invalidInputRule
    (validInput (validity 1))
    =>
    (printout t "You might have entered invalid values as inputs. Please enter again" crlf)
)

(reset)
(facts)
(run)