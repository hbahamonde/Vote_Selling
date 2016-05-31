* THis here is to clean the Qualtrics Data Set for the US Sample on vote-selling
use "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/data.dta", clear 

rename *, lower
keep age gender reg partyid  trust* knowledge1-knowledge6 q230 q230 q221 q223 q219 cj_1-cj_5 f_* socideo ecideo state zip educ relig race* marital income tknow6_3 q232_1_0 q232_2_0

rename trust_1 trustfed
rename trust_2 trustrep
rename trust_3 trustdem

rename knowledge1 roberts
rename knowledge2 supremecourt
rename knowledge3 vp
rename knowledge4 veto
rename knowledge5 majority
rename knowledge6 conservative

rename q232_1_0 pricecheap
rename q232_2_0 priceexpensive


recode roberts (2=0) (3=0) (4=0) 
label drop A_5E
recode supremecourt (1=0) (2=0) (3=1) (99=0)
label drop A_5J
recode vp (2=0) (3=0) (4=0) (99=0)
label drop A_5O
recode veto (2=0) (3=0) (99=0)
label drop A_5T
recode majority (99=0)
label drop A_5Y
recode conservative (99=0)
label drop A_63

rename q261_3 supremecourt_t
rename tknow3_3 vp_t
rename tknow4_3 veto_t
rename tknow5_3 majority_t
rename tknow6_3 conservative_t

rename q230 sell1
rename q221 treatment100
rename q223 treatment500
rename q219 control



label define race_1 1 "amindian"
label define race_2 1 "asian"
label define race_3 1 "black"
label define race_4 1 "hisp"
label define race_5 1 "hawaii"
label define race_6 1 "white"
label define race_7 1 "other"
label  values race_1  race_1
label  values race_2  race_2
label  values race_3  race_3
label  values race_4  race_4
label  values race_5  race_5
label  values race_6  race_6
label  values race_7  race_7

drop tknow1_1 tknow1_2 tknow1_4 q261_1 q261_2 q261_4 tknow3_1 tknow3_2 tknow3_4 tknow4_1 tknow4_2 tknow4_4 tknow5_1 tknow5_2 tknow5_4 q249_1 q249_2 q249_4 q246_3_1 q247_3_1 q249_3_0 q250_3_0  q246_1_1 q246_2_1 q246_4_1 q247_1_1 q247_2_1 q247_4_1 q249_1_0 q249_2_0 q249_4_0 q250_1_0 q250_2_0 q250_4_0 

* f_*  

*cj_*

saveold "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/data_list.dta", replace


***
use "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/data.dta", clear 

rename *, lower

keep f_*_*_* cj_1 cj_2 cj_3 cj_4 cj_5 

order f_1* cj_1 f_2* cj_2 f_3* cj_3 f_4* cj_4 f_5* cj_5 



saveold "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/data_cj.dta", replace

clear all


