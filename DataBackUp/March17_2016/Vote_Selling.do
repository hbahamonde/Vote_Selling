* List Experiment: Data Cleanin for R analyses

clear all
use "/Users/hectorbahamonde/RU/Term5/Experiments_Redlawsk/Experiment/Data/Qualtrics/Vote_Selling.dta"
save "/Users/hectorbahamonde/RU/Term5/Experiments_Redlawsk/Experiment/Data/Qualtrics/dat.dta", replace


* cleaning
capture drop if text2 == . | text7 == . | text8 == . | democrat == .
capture drop V2 V3 V4 V5 V7 V10 text1 text2 text5 time_* LocationAccuracy
drop text6 text7 text8 text3 text4 text10 Location*
drop if V1 == "R_28UTsCi0q1kZnip" | V1 == "R_1AFLlMHtDodgQk9" | V1 == "R_2azSiUceO5FoliV" /* voting tests*/
drop V1 V6 V8 V9

* creating treat variable
generate treat = listt if listt != .
replace treat = 1 if  treat  != .
replace treat = 0 if  treat == .

* renaming
rename text9 voteselling1
rename sure voteselling2
rename age_1_TEXT age

* generate ycount
egen ycount = rowtotal(listt listc)

* generate voteselling (direct question)
egen voteselling = rowtotal(voteselling1 voteselling2)

*ordering data set
order treat listc listt ycount 

* macro to convert 99 into dots
ds, has(type numeric)
foreach v in `r(varlist)' {
	replace `v' = . if `v'==99
	}


recode democrat (.=3)
recode buying1 (1=100) (2=300) (3=500) (4=800) (5=1000) (6=1500) (7=2000)
recode buying2 (1=100) (2=300) (3=500) (4=800) (5=1000) (6=1500) (7=2000)


recode ycount (4=3) (3=4) if treat == 1


* getting ready for R
saveold "/Users/hectorbahamonde/RU/Term5/Experiments_Redlawsk/Experiment/Data/Qualtrics/dat.dta", replace
