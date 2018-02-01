// change missing from 9 to .

generate breast82 = BRCANCER82
replace breast82 = . if BRCANCER82 == 9
replace breast82 = 0 if BRCANCER82 == 2

generate breast92 = BRCANCER92
replace breast92 = . if BRCANCER92 == 9
replace breast92 = 0 if BRCANCER92 == 2

generate breast86 = BRCANCER86
replace breast86 = . if BRCANCER86 == 9
replace breast86 = 0 if BRCANCER86 == 2

generate breast87 = BRCANCER87
replace breast87 = . if BRCANCER87 == 9
replace breast87 = 0 if BRCANCER87 == 2

// generate prevalence breast cancer if reported yes cancer in 71 or 82
generate PREV_BR = 0
replace PREV_BR = 1 if CANCER71 == 1 | breast82 == 1

// generate incidence breast cancer as reporting yes in 86, 87, 92 or died of breast cancer. Made missing those with prevalent breast cancer as defined above.
generate INC_BR = 0
replace INC_BR = 1 if breast92 == 1 | breast87 == 1 | breast86 == 1 | DEATHCODE == 8
replace INC_BR = . if PREV_BR ==1

//generate person time variable
gen ENDDATE=min(LSTKNDT, DATEDEATH, DATEDXBRCA82, DATEDXBRCA86, DATEDXBRCA87, DATEDXBRCA92, DATEDXCANCER71)

summarize ENDDATE
summarize ENDDATE, detail

gen PT = ENDDATE-DOExam
gen PY = PT/365.25
gen DATECHECK2=trunc(( LSTKNDT- DOExam)/30)
drop if PT <= 0

tab INC_BR

// generate rate of breast cancer
gen BR_RATE=INC_BR/PT

//relabel
generate sex = 0
replace sex = 1 if SEX == 2
label define sex2 0 "Male" 1 "Female"
label values sex sex2

// generate dichotomous BMI categories with normal (2) as reference
generate low_bmi = 0 if BMI_CAT == 2
replace low_bmi = 1 if BMI_CAT == 1

generate overweight = 0 if BMI_CAT == 2
replace overweight = 1 if BMI_CAT == 3

generate obese = 0 if BMI_CAT == 2
replace obese = 1 if BMI_CAT == 4

// determine if prevalence breast cancer is related to BMI
tab PREV_BR obese, chi2
tab PREV_BR overweight, chi2
tab PREV_BR low_bmi, chi2

// determine if incident breast cancer is related to BMI
ir INC_BR low_bmi PY
ir INC_BR obese PY
ir INC_BR overweight PY

// table of person time for incident breast cancer by variables
table BMI_CAT, by(INC_BR) c(sum PY)
table AGE_CAT, by(INC_BR) c(sum PY)
table RACE, by(INC_BR) c(sum PY)
table sex, by(INC_BR) c(sum PY)

// generate rate of breast cancer by age at exam
tabulate AGEEXAM INC_BR,row matcell(X)

m : 
    X = st_matrix("X")
    X = X, (X[., 2] :/ rowsum(X))
    st_matrix("X", X)
end

matlist X

// determine if missing variables influence the outcome by creating a new incidence variable 
generate INC_BR2 = 0

// include missing in new variable
replace INC_BR2 = 1 if breast82 == 1 & . | breast92 == 1 & . | breast87 == 1 & . | breast86 == 1 & . | DEATHCODE == 8 & .
replace INC_BR2 = . if PREV_BR ==1

// make missing = zero in new incidence variable
generate INC_BR3 = INC_BR2
replace INC_BR3 = 0 if INC_BR2 == .

// determine if new incidence varaible is related to BMI and how this influences the relationship
ir INC_BR3 low_bmi PY
ir INC_BR3 obese PY
ir INC_BR3 overweight PY

// determine what variables are independent risk factors for incident breast cancer
ir INC_BR twenties PY
ir INC_BR thirties PY
ir INC_BR fourties PY
ir INC_BR fifties PY
ir INC_BR sixties PY
ir INC_BR seventies PY

ir INC_BR EDUC2 PY
ir INC_BR EDUC3 PY
ir INC_BR EDUC4 PY

ir INC_BR ALC2 PY
ir INC_BR ALC3 PY
ir INC_BR ALC4 PY

ir INC_BR PHYSACT2 PY
ir INC_BR PHYSACT3 PY
ir INC_BR PHYSACT4 PY

ir INC_BR SMOKE_PAST PY
ir INC_BR SMOKE3 PY
ir INC_BR SMOKE4 PY

ir INC_BR WHITE PY
ir INC_BR BLACK PY
ir INC_BR OTHER PY

ir INC_BR FAM_INC2 PY
ir INC_BR FAM_INC3 PY
ir INC_BR FAM_INC4 PY

// create a new age category variable with wider bands
generate AGECAT2 = AGE_CAT
replace AGECAT2 = 0 if AGE_CAT == 1 // 0 = 20s and 30s
replace AGECAT2 = 1 if AGE_CAT == 2 // 1 = 40s and 50s
replace AGECAT2 = 1 if AGE_CAT == 3 
replace AGECAT2 = 2 if AGE_CAT == 4 // 2 = 60s and 70s
replace AGECAT2 = 2 if AGE_CAT == 5

// see how new age category influences breast cancer
logistic INC_BR i.AGECAT2 

// saturated model
logistic INC_BR ib2.BMI_CAT i.AGECAT2 i.INCOME i. EDUC_CAT i.PHYS_ACT71 BMI_CAT#RACE BMI_CAT#AGECAT2
logistic INC_BR ib2.BMI_CAT i.AGE_CAT i.INCOME i. EDUC_CAT i.PHYS_ACT71 BMI_CAT#RACE BMI_CAT#AGE_CAT

poisson INC_BR ib2.BMI_CAT i.AGE_CAT i.INCOME i. EDUC_CAT i.PHYS_ACT71 BMI_CAT#RACE BMI_CAT#AGE_CAT, exposure(PY) irr
poisson INC_BR ib2.BMI_CAT i.AGECAT2 i.INCOME i. EDUC_CAT i.PHYS_ACT71 BMI_CAT#RACE BMI_CAT#AGECAT2, exposure(PY) irr

stset PY, failure(INC_BR ==1) // set failure and survival

stcox INC_BR ib2.BMI_CAT i.AGECAT2 i.INCOME i. EDUC_CAT i.PHYS_ACT71 BMI_CAT#RACE BMI_CAT#AGECAT2
stcox INC_BR ib2.BMI_CAT i.AGE_CAT i.INCOME i. EDUC_CAT i.PHYS_ACT71 BMI_CAT#RACE BMI_CAT#AGE_CAT

// parsimonious model
logistic INC_BR ib2.BMI_CAT i.AGECAT2 BMI_CAT#RACE BMI_CAT#AGECAT2
logistic INC_BR ib2.BMI_CAT i.AGE_CAT BMI_CAT#RACE BMI_CAT#AGE_CAT

poisson INC_BR ib2.BMI_CAT i.AGE_CAT BMI_CAT#RACE BMI_CAT#AGE_CAT, exposure(PY) irr
poisson INC_BR ib2.BMI_CAT i.AGECAT2 BMI_CAT#RACE BMI_CAT#AGECAT2, exposure(PY) irr

stcox INC_BR ib2.BMI_CAT i.AGECAT2 BMI_CAT#RACE BMI_CAT#AGECAT2
stcox INC_BR ib2.BMI_CAT i.AGE_CAT BMI_CAT#RACE BMI_CAT#AGE_CAT
