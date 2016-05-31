* example script to implement estimators of Average Marginal Component Effects (ACMEs) for Conjoint Data
* developed in :
* Causal Inference in Conjoint Analysis:
* Understanding Multidimensional Choices via Stated Preference Experiments
* Jens Hainmueller, Daniel Hopkins, Teppei Yamamoto

* load example data
use conjoint.dta, clear 

* this is a subset of the immigration conjoint reported in the paper
* Three Immigrant Attributes: Language Skills, Education, Occupation
* Outcome: Immigrant Profile Preferred or not

/* ACME for attribute without randomization restriction */

* Immigrant's language skill is randomly 
tab FeatLang
* estimate ACME using regression
reg Chosen_Immigrant i.FeatLang , cl( CaseID )
* ACME of going from fluent English to using an interpreter is -.16 


/* ACME for attribute with randomization restriction */
* Immigrant's education and jobs are restricted:
* high skilled jobs (Financial analyst, Computer programmer, Research scientist, Doctor) 
* are only allowed with high levels of education (two year college or higher)
tab FeatJob FeatEd 

* Estimate ACMEs using regression with interactions
reg Chosen_Immigrant i.FeatEd##i.FeatJob , cl( CaseID )

* Example: ACME of going from no formal (FeatEd=1) to college degree (FeatEd=6)
* by computing weighted average of educ effect in each non-emtpty job stratum
* since high skilled jobs are not valid for no formal education, we ony average over low skilled job strata
* and exclude high skilled jobs (i.e. numbers 5 Financial analyst, 8 Computer programmer, 10 Research scientist, and 11 Doctor)
lincom  (6.FeatEd * 1/7  + (6.FeatEd + 6.FeatEd#2.FeatJob)  * 1/7 + /// 
                           (6.FeatEd + 6.FeatEd#3.FeatJob)  * 1/7 + ///
                           (6.FeatEd + 6.FeatEd#4.FeatJob)  * 1/7 + ///
            			   (6.FeatEd + 6.FeatEd#5.FeatJob)  * 0 + ///
						   (6.FeatEd + 6.FeatEd#6.FeatJob)  * 1/7 + ///
						   (6.FeatEd + 6.FeatEd#7.FeatJob)  * 1/7 + ///
						   (6.FeatEd + 6.FeatEd#8.FeatJob)  * 0 + ///
						   (6.FeatEd + 6.FeatEd#9.FeatJob)  * 1/7 + ///
						   (6.FeatEd + 6.FeatEd#10.FeatJob) * 0 + ///
						   (6.FeatEd + 6.FeatEd#11.FeatJob) * 0)

* same as						   
lincom  6.FeatEd + (6.FeatEd#2.FeatJob + 6.FeatEd#3.FeatJob + ///
                    6.FeatEd#4.FeatJob + 6.FeatEd#6.FeatJob + ///
					6.FeatEd#7.FeatJob + 6.FeatEd#9.FeatJob)  * 1/7 

						   
* Example: ACME of going from 1 Janitor to 2 Waiter
* Since both jobs go with all educ levels we average job effect across all strata
lincom  (2.FeatJob *1/7    + (2.FeatJob + 2.FeatEd#2.FeatJob) *1/7   + /// 
                             (2.FeatJob + 3.FeatEd#2.FeatJob) *1/7   + ///
                             (2.FeatJob + 4.FeatEd#2.FeatJob) *1/7   + ///
						     (2.FeatJob + 5.FeatEd#2.FeatJob) *1/7   + ///
						     (2.FeatJob + 6.FeatEd#2.FeatJob) *1/7   + ///
						     (2.FeatJob + 7.FeatEd#2.FeatJob) *1/7 )								  

* Example: ACME of going from 1 Janitor to 11 Doctor
* Since Doctor only goes with high educ levels we average job effect across all high educ strata							 
lincom  (11.FeatJob *0    + (11.FeatJob + 2.FeatEd#11.FeatJob) *0   + /// 
                            (11.FeatJob + 3.FeatEd#11.FeatJob) *0   + ///
                            (11.FeatJob + 4.FeatEd#11.FeatJob) *0   + ///
			     		    (11.FeatJob + 5.FeatEd#11.FeatJob) *1/3   + ///
						    (11.FeatJob + 6.FeatEd#11.FeatJob) *1/3   + ///
						    (11.FeatJob + 7.FeatEd#11.FeatJob) *1/3 )	
							
/* ACMEs with Single Regression */
* Notice that we can also estimate ACMEs simulatenously in one regression
* which yields the same results in expectation
reg Chosen_Immigrant i.FeatLang i.FeatEd##i.FeatJob , cl( CaseID )
* ACME of going from fluent English to using an interpreter is -.16 



							
							