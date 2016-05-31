# example script to implement estimators of Average Marginal Component Effects (ACMEs) for Conjoint Data
# developed in :
# Causal Inference in Conjoint Analysis:
# Understanding Multidimensional Choices via Stated Preference Experiments
# Jens Hainmueller, Daniel Hopkins, Teppei Yamamoto

# function that does clustered SEs
vcovCluster <- function(
  model,
  cluster
)
{
  require(sandwich)
  require(lmtest)
  if(nrow(model.matrix(model))!=length(cluster)){
    stop("check your data: cluster variable has different N than model")
  }
  M <- length(unique(cluster))
  N <- length(cluster)           
  K <- model$rank   
  if(M<50){
    warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
  }
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  return(rcse.cov)
}

# load example data
library(foreign)
library(lmtest)
library(sandwich)
library(msm)
d <- read.dta("conjoint.dta")
head(d)
d$Chosen_Immigrant <- as.numeric(d$Chosen_Immigrant)

# this is a subset of the immigration conjoint reported in the paper
# Three Immigrant Attributes: Language Skills, Education, Occupation
# Outcome: Immigrant Profile Preferred or not

## ACME for attribute without randomization restriction 
  
# Immigrant's language skill is randomly 
table(d$FeatLang)

# estimate ACME using regression
lmout <- lm(Chosen_Immigrant~FeatLang,data=d)
coeftest(lmout, vcov = vcovCluster(lmout, cluster = d$CaseID))
# AMCE of going from fluent English to using an interpreter is -.16 


## ACME for attribute with randomization restriction */
# Immigrant's education and jobs are restricted:
# high skilled jobs (Financial analyst, Computer programmer, Research scientist, Doctor) 
# are only allowed with high levels of education (two year college or higher)
table(d$FeatJob,d$FeatEd)

# Estimate ACMEs using regression with interactions
lmout <- lm(Chosen_Immigrant~FeatEd*FeatJob,data=d)

# get coefs and VCOV matrix for delta method
estmean <- na.omit(coef(lmout))
estvar  <- vcovCluster(lmout, cluster = d$CaseID)

# Example: ACME of going from no formal (FeatEd=1) to college degree (FeatEd=6)
# by computing weighted average of educ effect in each non-emtpty job stratum
# since high skilled jobs are not valid for no formal education, we ony average over low skilled job strata
# and exclude high skilled jobs (i.e. numbers 5 Financial analyst, 8 Computer programmer, 10 Research scientist, and 11 Doctor)

# get terms we need for effect
terms <- c("FeatEdcollege degree",
"FeatEdcollege degree:FeatJobWaiter",
"FeatEdcollege degree:FeatJobChild care provider",
"FeatEdcollege degree:FeatJobGardener",           
"FeatEdcollege degree:FeatJobConstruction worker",
"FeatEdcollege degree:FeatJobTeacher",
"FeatEdcollege degree:FeatJobNurse"
)
# get positions
pos <- (1:length(estmean))[names(estmean) %in% terms ]
names(estmean)[pos]
# effect formula
g <- as.formula(paste("~",paste(paste("x",pos[1],"+",sep="")),
                          paste("1/7*(",
                          paste("x",pos[2:length(pos)],sep="",collapse="+")
                                ,")",sep="")
                      ))

# ACME estimate
estmean[terms][1]+sum(estmean[terms][2:length(terms)])*1/7
# SE of ACME
deltamethod(g, estmean, estvar, ses=TRUE)


# Example: ACME of going from 1 Janitor to 2 Waiter
# Since both jobs go with all educ levels we average job effect across all strata
terms <- c("FeatJobWaiter",
           "FeatEd4th grade:FeatJobWaiter",                    
           "FeatEd8th grade:FeatJobWaiter",                    
           "FeatEdhigh school:FeatJobWaiter",                  
           "FeatEdtwo-year college:FeatJobWaiter",             
           "FeatEdcollege degree:FeatJobWaiter" ,              
           "FeatEdgraduate degree:FeatJobWaiter"
)
# get positions
pos <- (1:length(estmean))[names(estmean) %in% terms ]
names(estmean)[pos]
# effect formula
g <- as.formula(paste("~",paste(paste("x",pos[1],"+",sep="")),
                      paste("1/7*(",
                            paste("x",pos[2:length(pos)],sep="",collapse="+")
                            ,")",sep="")
))

# ACME estimate
estmean[terms][1]+sum(estmean[terms][2:length(terms)])*1/7
# SE of ACME
deltamethod(g, estmean, estvar, ses=TRUE)


# Example: ACME of going from 1 Janitor to 11 Doctor
# Since Doctor only goes with high educ levels we average job effect across all high educ strata  						 

terms <- c("FeatJobDoctor",
           "FeatEdtwo-year college:FeatJobDoctor",             
           "FeatEdcollege degree:FeatJobDoctor" 
)
# get positions
pos <- (1:length(estmean))[names(estmean) %in% terms ]
names(estmean)[pos]
# effect formula
g <- as.formula(paste("~",paste(paste("x",pos[1],"+",sep="")),
                      paste("1/3*(",
                            paste("x",pos[2:length(pos)],sep="",collapse="+")
                            ,")",sep="")
))

# ACME estimate
estmean[terms][1]+sum(estmean[terms][2:length(terms)])*1/3
# SE of ACME
deltamethod(g, estmean, estvar, ses=TRUE)
                                                                                                      
## ACMEs with Single Regression 
# Notice that we can also estimate ACMEs simulatenously in one regression
# which yields the same results in expectation
lm(Chosen_Immigrant~FeatEd*FeatJob+FeatLang,data=d)
# ACME of going from fluent English to using an interpreter is -.16 
                                                                                                          
                                                                                                          
                                                                                                          
                                                                                                          
                                                                                                          


