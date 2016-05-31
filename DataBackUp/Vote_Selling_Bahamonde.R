############################## 
# Cleaning
##############################
cat("\014")
rm(list=ls())


# Load the data
library(foreign)
dat <- read.dta("/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/data_list.dta")

# Data Cleaning
## Drop if Treatments are Missing

dat = dat[!with(dat,is.na(treatment100) & is.na(treatment500) & is.na(control) |
                        is.na(cj_1) | is.na(cj_2) | is.na(cj_3) | is.na(cj_4) | is.na(cj_5)
                ),]
#dat = dat[!with(dat,is.na(treatment100) & is.na(treatment500) & is.na(control)),]
## Combine treatment100 & treatment500 & control
dat$ycount = rowSums(data.frame(cbind(dat$control, dat$treatment100, dat$treatment500)), na.rm = T)
## Create treatment dummy variable
dat$treatment[dat$treatment100 != "NA" | dat$treatment500 != "NA"] <- 1 ; dat$treatment[is.na(dat$treatment)] <- 0
# Dummy for the treatment they got
dat$treatlow[dat$treatment100 != "NA"] <- 1 ; dat$treatlow[is.na(dat$treatlow)] <- 0


# Recoding Race
dat$white[dat$race_1 != "NA" | dat$race_2 != "NA" | dat$race_3 != "NA" | dat$race_4 != "NA" | dat$race_5 != "NA"] <- 0 ; dat$white[is.na(dat$white)] <- 1
# Renaming Religion
names(dat)[names(dat) == "relig"] <- "religion"

# renaming male
names(dat)[names(dat) == "gender"] <- "woman" 

# recoding sell (direct sell question)
names(dat)[names(dat) == "sell1"] <- "directquestion" # renaming
dat$directquestion.f <- ordered(dat$directquestion, levels = c(#recoding
        "No, I don't want to sell my vote", 
        "Yes, I want to sell my vote"), labels = c("No", "Yes"))
library(car)
dat$directquestion  <- recode(as.integer(dat$directquestion), "1 = 0 ; 2 = 1")

# Combine (weighted) pol. knowledge vars
dat$polknow = rowSums(data.frame(cbind(
        dat$roberts/dat$tknow1_3, 
        dat$supremecourt/dat$supremecourt_t, 
        dat$vp/dat$vp_t, 
        dat$veto/dat$veto_t,
        dat$majority/dat$majority_t,
        dat$conservative/dat$conservative_t)), na.rm = T)

# Merge ZIP data
zipdata <- read.dta("/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/zipdata.dta") # import ZIP DTA DATA 
zipdata[zipdata == -1] <- NA
dat = merge(dat, zipdata, by=c("zip"), all.x =T)

## Transforming Vars

levels(dat$socideo)[levels(dat$socideo)=="Very liberal"] <- "VeryLiberal"
levels(dat$socideo)[levels(dat$socideo)=="Very conservative"] <- "VeryConservative"
levels(dat$partyid)[levels(dat$partyid)=="Something else"] <- "SomethingElse"
levels(dat$reg)[levels(dat$reg)=="Now registered to vote"] <- "Registered"
levels(dat$reg)[levels(dat$reg)=="Haven't been able to register so far"] <- "Unregistered"
levels(dat$trustfed)[levels(dat$trustfed)=="No Trust at All"] <- "NoTrustAtAll"
levels(dat$trustfed)[levels(dat$trustfed)=="Not Very Much Trust"] <- "NotVeryMuchTrust"
levels(dat$trustfed)[levels(dat$trustfed)=="Indifferent/In the Middle"] <- "Indifferent"
levels(dat$trustfed)[levels(dat$trustfed)=="Fair Amount of Trust"] <- "FairAmountOfTrust"
levels(dat$trustfed)[levels(dat$trustfed)=="A Great Deal of Trust"] <- "AGreatDealOfTrust"
levels(dat$educ)[levels(dat$educ) == "Some high school"] <- "SomeHighSchool"
levels(dat$educ)[levels(dat$educ) == "High school graduate (high school diploma or equivalent GED)"] <- "HighSchoolGraduate"
levels(dat$educ)[levels(dat$educ) == "Vocational/technical school"] <- "TechnicalSchool"
levels(dat$educ)[levels(dat$educ) == "Some college"] <- "SomeCollege"
levels(dat$educ)[levels(dat$educ) == "Associate degree (usually two years of college)"] <- "AssociateDegree"
levels(dat$educ)[levels(dat$educ) == "Bachelor's degree (usually four years of college)"] <- "BachelorsDegree"
levels(dat$educ)[levels(dat$educ) == "Graduate work (including a Master's degree, law or medical school, or PhD)"] <- "GraduateSchool"


#dat <- dat[complete.cases(dat$woman),]
#dat <- dat[complete.cases(dat$socideo),]
#dat <- dat[complete.cases(dat$partyid),]
#dat <- dat[complete.cases(dat$reg),]
#dat <- dat[complete.cases(dat$trustfed),]
#dat <- dat[complete.cases(dat$income),]
#dat <- dat[complete.cases(dat$educ),]
#dat <- dat[complete.cases(dat$polknow),]
#dat <- dat[complete.cases(dat$ziplabforce),]
#dat <- dat[complete.cases(dat$zippercamincome),]

dat$age.n = as.numeric(dat$age) 
dat$income.n = as.numeric(dat$income)
dat$educ.n = as.numeric(dat$educ)


#dat$proplabforgovtwork = dat$zipgovtworkers / dat$ziplabforc # Ratio Govt Workers/Labor Force ZIP level
#dat <- dat[complete.cases(dat$proplabforgovtwork),]

#dat$sizeofthepoor = rowSums(data.frame(dat$zipless10k,dat$zip1015k,dat$zip1525k)) / dat$ziplabforce # size of the poor

#dat$sizetimesincome = dat$sizeofthepoor*dat$income.n # predict.ictreg doesn't take well interactions.
#dat <- dat[complete.cases(dat$sizetimesincome),]

#dat$zipinequality = as.numeric(dat$zipmeanincome - dat$zipmedianincome)
#dat <- dat[complete.cases(dat$zipinequality),]

# Saving Data
save(dat, file = "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_list.RData") # in paper's folder

############################## 
# CONJOINT Experiment DATA CLEANING
##############################
cat("\014")
rm(list=ls())

# C
library(foreign)
c <- read.dta("/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/data.dta")

### change names
colnames(c)[which(names(c) == "CJ_1")] <- "cj_1"
colnames(c)[which(names(c) == "CJ_2")] <- "cj_2"
colnames(c)[which(names(c) == "CJ_3")] <- "cj_3"
colnames(c)[which(names(c) == "CJ_4")] <- "cj_4"
colnames(c)[which(names(c) == "CJ_5")] <- "cj_5"


# clean data set: complete cases in CONJOINT and LIST treatments
c = c[!with(c,is.na(Q221) & is.na(Q223) & is.na(Q219) |
                    is.na(cj_1) | is.na(cj_2) | is.na(cj_3) | is.na(cj_4) | is.na(cj_5)
),]
c = subset(c, select = c("cj_1", "cj_2", "cj_3", "cj_4", "cj_5", "F_1_1_1",  "F_1_1_2", "F_1_1_3", "F_1_1_4", "F_1_1_5", "F_1_2_1", "F_1_2_2", "F_1_2_3", "F_1_2_4", "F_1_2_5", "F_2_1_1",  "F_2_1_2", "F_2_1_3", "F_2_1_4", "F_2_1_5", "F_2_2_1", "F_2_2_2", "F_2_2_3", "F_2_2_4", "F_2_2_5", "F_3_1_1", "F_3_1_2", "F_3_1_3", "F_3_1_4", "F_3_1_5", "F_3_2_1", "F_3_2_2", "F_3_2_3", "F_3_2_4", "F_3_2_5", "F_4_1_1", "F_4_1_2", "F_4_1_3", "F_4_1_4", "F_4_1_5", "F_4_2_1", "F_4_2_2", "F_4_2_3", "F_4_2_4", "F_4_2_5", "F_5_1_1", "F_5_1_2", "F_5_1_3", "F_5_1_4", "F_5_1_5", "F_5_2_1", "F_5_2_2", "F_5_2_3", "F_5_2_4", "F_5_2_5"))

# change names again
all_d = subset(c, select = c("cj_1", "cj_2", "cj_3", "cj_4", "cj_5"))
colnames(all_d)[which(names(all_d) == "cj_1")] <- "d1"
colnames(all_d)[which(names(all_d) == "cj_2")] <- "d2"
colnames(all_d)[which(names(all_d) == "cj_3")] <- "d3"
colnames(all_d)[which(names(all_d) == "cj_4")] <- "d4"
colnames(all_d)[which(names(all_d) == "cj_5")] <- "d5"

c$idnum = rep(1:nrow(c))
nrowc = nrow(c) # I will use this here to entry the number of VALID subjects I've got

# gen aux dataset with the choices and idnum
#candidate.d = data.frame(c$idnum, c$cj_1, c$cj_2, c$cj_3, c$cj_4, c$cj_5)
#colnames(candidate.d) = c("idnum", "choice1", "choice2", "choice3", "choice4", "choice5")

# leave main dataframe with just the attributes and idnum
c = subset(c, select = -c(cj_1, cj_2, cj_3, cj_4, cj_5) )

# reshape dataset vertically
library(reshape)
c = melt(c, id="idnum")

# create code variable
c$variable = as.character(c$variable)
library(stringr)
c.string = data.frame(str_split_fixed(c$variable, "_", 4))
colnames(c.string) = c("drop", "pair", "candidate", "attribute")

# merge c.string and c dataframes
c = data.frame(c, c.string)
c = subset(c, select = -c(drop, variable, attribute))

# generate a variable which writes the TYPE OF ATTRIBUTE
c$type = ifelse(c$value == "Citizens CAN associate with others and form groups", "RightToAssociate", 
                ifelse(c$value == "Citizens CANNOT associate with others and form groups", "RightToAssociate", 
                       ifelse(c$value == "Citizens CAN run for office for the next two elections", "RightToRun", 
                              ifelse(c$value == "Citizens CANNOT run for office for the next two elections", "RightToRun", 
                                     ifelse(c$value == "Citizens CAN vote in the next two elections", "RightToVote", 
                                            ifelse(c$value == "Citizens CANNOT vote in the next two elections", "RightToVote", 
                                                   ifelse(c$value == "Media CAN confront the Government", "FreePress", 
                                                          ifelse(c$value == "Media CANNOT confront the Government", "FreePress", 
                                                                 ifelse(c$value == "President CAN rule without Congress", "PresAutonomy", 
                                                                        ifelse(c$value == "President CANNOT rule without Congress", "PresAutonomy", NA
                                                                        ))))))))))

###
c$code = paste(c$idnum,c$pair,c$candidate,sep = "-")
colnames(c)[2] <- "caracteristica"
colnames(c)[5] <- "descripcion"
c$descripcion = as.character(c$descripcion)
c$caracteristica = as.character(c$caracteristica)
# c <- c[order(c$idnum),] ; rownames(c) <- NULL




# D (empty dataset)
idnum = rep(1:nrowc, each = 10)
pair = rep(c(1,1,2,2,3,3,4,4,5,5), times = nrowc) # this is the number of TASKS
candidate = rep(c(1,2), times = (nrowc*10)/2)
at.run = rep(NA,nrowc*10)
at.asso = rep(NA,nrowc*10)
at.press = rep(NA,nrowc*10)
at.presaut = rep(NA,nrowc*10)
at.vote = rep(NA,nrowc*10)
selected = as.character(rep(NA,nrowc*10))
code = paste(idnum,pair,candidate,sep = "-")
d = data.frame(code,idnum,pair,candidate,at.run, at.asso, at.press, at.presaut, at.vote,selected) # conjoint dataset


# Loops to populate d dataset
for (i in code) { # RightToRun
        d$at.run[d$code==i] = c$caracteristica[c$descripcion=="RightToRun" & c$code==i]
        }

for (i in code) { # RightToAssociate
        d$at.asso[d$code==i] = c$caracteristica[c$descripcion=="RightToAssociate" & c$code==i]
}


for (i in code) { # FreePress
        d$at.press[d$code==i] = c$caracteristica[c$descripcion=="FreePress" & c$code==i]
}


for (i in code) { # PresAutonomy
        d$at.presaut[d$code==i] = c$caracteristica[c$descripcion=="PresAutonomy" & c$code==i]
}


for (i in code) { # RightToVote
        d$at.vote[d$code==i] = c$caracteristica[c$descripcion=="RightToVote" & c$code==i]
}

# Generate "outcome" dataset

# E
load("/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_list.RData")

e = dat

idnum.e = rep(1:nrow(e), times = 10)
pair.e = rep(c(1,1,2,2,3,3,4,4,5,5), each = nrow(e)) # this is the number of TASKS
candidate.e = rep(c(1,2), each = nrow(e), times = 5)
code.e = paste(idnum.e, pair.e, candidate.e, sep = "-")

woman.e = c(e$woman,e$woman,e$woman,e$woman,e$woman,e$woman,e$woman,e$woman,e$woman,e$woman)
socideo.e = c(e$socideo,e$socideo,e$socideo,e$socideo,e$socideo,e$socideo,e$socideo,e$socideo,e$socideo,e$socideo)
partyid.e = c(e$partyid,e$partyid,e$partyid,e$partyid,e$partyid,e$partyid,e$partyid,e$partyid,e$partyid,e$partyid)
reg.e = c(e$reg,e$reg,e$reg,e$reg,e$reg,e$reg,e$reg,e$reg,e$reg,e$reg)
trustfed.e = c(e$trustfed,e$trustfed,e$trustfed,e$trustfed,e$trustfed,e$trustfed,e$trustfed,e$trustfed,e$trustfed,e$trustfed)
income.n.e = c(e$income,e$income,e$income,e$income,e$income,e$income,e$income,e$income,e$income,e$income)
educ.n.e = c(e$educ,e$educ,e$educ,e$educ,e$educ,e$educ,e$educ,e$educ,e$educ,e$educ)
polknow.e = c(e$polknow,e$polknow,e$polknow,e$polknow,e$polknow,e$polknow,e$polknow,e$polknow,e$polknow,e$polknow)
zipinequality.e = c(e$zipinequality,e$zipinequality,e$zipinequality,e$zipinequality,e$zipinequality,e$zipinequality,e$zipinequality,e$zipinequality,e$zipinequality,e$zipinequality)
sizeofthepoor.e = c(e$sizeofthepoor,e$sizeofthepoor,e$sizeofthepoor,e$sizeofthepoor,e$sizeofthepoor,e$sizeofthepoor,e$sizeofthepoor,e$sizeofthepoor,e$sizeofthepoor,e$sizeofthepoor)
proplabforgovtwork.e = c(e$proplabforgovtwork,e$proplabforgovtwork,e$proplabforgovtwork,e$proplabforgovtwork,e$proplabforgovtwork,e$proplabforgovtwork,e$proplabforgovtwork,e$proplabforgovtwork,e$proplabforgovtwork,e$proplabforgovtwork)


# match with D
d$woman = rep(NA, nrowc*10)
d$socideo = rep(NA, nrowc*10)
d$partyid = rep(NA, nrowc*10)
d$reg = rep(NA, nrowc*10)
d$trustfed = rep(NA, nrowc*10)
d$income.n = rep(NA, nrowc*10)
d$educ.n = rep(NA, nrowc*10)
d$polknow = rep(NA, nrowc*10)
d$selected = rep(NA, nrowc*10)


d1.0 = c(e$cj_1)
d1 = c(d1.0,d1.0)
d2.0 = c(e$cj_2)
d2 = c(d2.0,d2.0)
d3.0 = c(e$cj_3)
d3 = c(d3.0,d3.0)
d4.0 = c(e$cj_4)
d4 = c(d4.0,d4.0)
d5.0 = c(e$cj_5)
d5 = c(d5.0,d5.0)

all_d = c(d1,d2,d3,d4,d5)

###



outcome = data.frame(code.e,idnum.e, pair.e, candidate.e,all_d, woman.e, socideo.e, partyid.e, reg.e, trustfed.e, income.n.e, educ.n.e, polknow.e)


# LOOPS: populating D dataset
for (i in code) {# selected
        d$selected2[d$code==i] = outcome$all_d[outcome$code.e==i] 
        }

for (i in code) {# woman
        d$woman[d$code==i] = outcome$woman[outcome$code==i]
        }

for (i in code) {# socideo
        d$socideo[d$code==i] = outcome$socideo[outcome$code==i]
}

for (i in code) {# partyid
        d$partyid[d$code==i] = outcome$partyid[outcome$code==i]
}

for (i in code) {# reg
        d$reg[d$code==i] = outcome$reg[outcome$code==i]
}

for (i in code) {# trustfed
        d$trustfed[d$code==i] = outcome$trustfed[outcome$code==i]
}

for (i in code) {# income.n
        d$income.n[d$code==i] = outcome$income.n[outcome$code==i]
}

for (i in code) {# educ.n
        d$educ.n[d$code==i] = outcome$educ.n[outcome$code==i]
}

for (i in code) {# polknow
        d$polknow[d$code==i] = outcome$polknow[outcome$code==i]
}


save(d, file = "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_cj.RData")

# selected
d$vote = ifelse(d$candidate == d$selected2, 1, 0)
d$selected = d$vote
d$selected2 = NULL
d$vote = NULL


# Saving Data
save(d, file = "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/mergedconjoint.RData")

############################## 
# CONJOINT Experiment DATA ANALYSES
##############################
cat("\014")
rm(list=ls())

# Load Data
load("/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/mergedconjoint.RData") # Load data


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


library(lmtest)
library(sandwich)
library(msm) # install.packages("msm")

# outcome variable NUMERIC
d$selected <- as.numeric(d$selected)

# exclude NAs from the Attributes columns
apply(is.na(d),2,sum) # I have several of them


# function to exclude NAs from the rows 
completeFun <- function(data, desiredCols) {
        completeVec <- complete.cases(data[, desiredCols])
        return(data[completeVec, ])
}
d = completeFun(d, c("at.run","at.asso","at.press","at.presaut", "at.vote")) # Attributes
d = completeFun(d, c("income.n", "socideo")) # Covariates


# single parameter models
at.run.out <- lm(selected ~ at.run, data=d)
at.asso.out <- lm(selected ~ at.asso, data=d)
at.press.out <- lm(selected ~ at.press, data=d)
at.presaut.out <- lm(selected ~ at.presaut, data=d)
at.vote.out <- lm(selected ~ at.vote, data=d)


# library(geepack)
## logit GEE
# options(scipen=999)
# at.run.out = geeglm(selected ~ at.run,
#                    family = binomial, 
#                     id = idnum, 
#                     corstr = "exchangeable",
#                     data = d)

# at.asso.out = geeglm(selected ~ at.asso,
#                      family = binomial, 
#                      id = idnum, 
#                      corstr = "exchangeable",
#                      data = d)

# at.press.out = geeglm(selected ~ at.press,
#                       family = binomial, 
#                       id = idnum, 
#                       corstr = "exchangeable",
#                       data = d)

# at.presaut.out = geeglm(selected ~ at.presaut,
#                         family = binomial, 
#                         id = idnum, 
#                         corstr = "exchangeable",
#                         data = d)

# at.vote.out = geeglm(selected ~ at.vote,
#                      family = binomial, 
#                      id = idnum, 
#                      corstr = "exchangeable",
#                      data = d)


at.run.std = coeftest(at.run.out, vcov = vcovCluster(at.run.out, cluster = d$idnum))
at.asso.std = coeftest(at.asso.out, vcov = vcovCluster(at.asso.out, cluster = d$idnum))
at.press.std = coeftest(at.press.out, vcov = vcovCluster(at.press.out, cluster = d$idnum))
at.presaut.std = coeftest(at.presaut.out, vcov = vcovCluster(at.presaut.out, cluster = d$idnum))
at.vote.std = coeftest(at.vote.out, vcov = vcovCluster(at.vote.out, cluster = d$idnum))



# auxiliary dataset 
at.run.out.d <- data.frame(Variable = rownames(summary(at.run.out)$coef),
                           Coefficient = summary(at.run.out)$coef[, 1],
                           SE = c(at.run.std[3],at.run.std[4]),
                           #SE = summary(at.run.out)$coef[, 2],
                           modelName = "Run for Office")


at.asso.out.d <- data.frame(Variable = rownames(summary(at.asso.out)$coef),
                            Coefficient = summary(at.asso.out)$coef[, 1],
                            SE = c(at.asso.std[3],at.asso.std[4]),
                            #SE = summary(at.asso.out)$coef[, 2],
                            modelName = "Right to Associate")

at.press.out.d <- data.frame(Variable = rownames(summary(at.press.out)$coef),
                             Coefficient = summary(at.press.out)$coef[, 1],
                             SE = c(at.press.std[3],at.press.std[4]),
                             #SE = summary(at.press.out)$coef[, 2],
                             modelName = "Free Press")

at.presaut.out.d <- data.frame(Variable = rownames(summary(at.presaut.out)$coef),
                               Coefficient = summary(at.presaut.out)$coef[, 1],
                               SE = c(at.presaut.std[3],at.presaut.std[4]),
                               #SE = summary(at.presaut.out)$coef[, 2],
                               modelName = "Presidents Autonomy")

at.vote.out.d <- data.frame(Variable = rownames(summary(at.vote.out)$coef),
                            Coefficient = summary(at.vote.out)$coef[, 1],
                            SE = c(at.vote.std[3],at.vote.std[4]),
                            #SE = summary(at.vote.out)$coef[, 2],
                            modelName = "Right to Vote")

fiveattributes <- data.frame(rbind(at.run.out.d, at.asso.out.d, at.press.out.d,at.presaut.out.d,at.vote.out.d))
fiveattributes$variable = NA
fiveattributes$variable = c("Citizens CAN run for office for the next two elections", 
                            "Citizens CANNOT run for office for the next two elections", 
                            "Citizens CAN associate with others and form groups", 
                            "Citizens CANNOT associate with others and form groups", 
                            "Media CAN confront the Government", 
                            "Media CANNOT confront the Government", 
                            "President CAN rule without Congress", 
                            "President CANNOT rule without Congress", 
                            "Citizens CAN vote in the next two elections", 
                            "Citizens CANNOT vote in the next two elections")

fiveattributes$Variable = NULL # drop var
rownames(fiveattributes) <- seq(length=nrow(fiveattributes)) # reset Row Names


# Plot
library(ggplot2)
ggplot(fiveattributes, aes(colour = modelName)) + 
        geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
        geom_linerange(aes(x = variable, 
                           ymin = Coefficient - SE*-qnorm((1-0.95)/2), 
                           ymax = Coefficient + SE*-qnorm((1-0.95)/2)
                           ),
        position = position_dodge(width = 1/2)) +
        geom_pointrange(aes(x = variable, 
                            y = Coefficient, 
                            ymin = Coefficient - SE*-qnorm((1-0.95)/2),
                            ymax = Coefficient + SE*-qnorm((1-0.95)/2)
                            ), 
                        shape = 21, 
                        fill = "WHITE"
                        ) +
        coord_flip() + 
        xlab("") + 
        ylab("Coefficient") +
        guides(colour=FALSE) +
        theme(legend.position="none") + 
        theme_bw() +
        #facet_grid(. ~ modelName) +
        xlim("Citizens CAN run for office for the next two elections", 
             "Citizens CANNOT run for office for the next two elections", 
             "Citizens CAN vote in the next two elections", 
             "Citizens CANNOT vote in the next two elections", 
             "Citizens CAN associate with others and form groups", 
             "Citizens CANNOT associate with others and form groups", 
             "Media CAN confront the Government", 
             "Media CANNOT confront the Government", 
             "President CAN rule without Congress", 
             "President CANNOT rule without Congress")





# estimate ACME using regression
lmout <- lm(selected ~ 
                    at.run*income.n + at.asso*income.n + at.press*income.n + at.presaut*income.n + at.vote*income.n +
                    at.run*polknow + at.asso*polknow + at.press*polknow + at.presaut*polknow + at.vote*polknow,
                    data=d)
#
lmout.hat = coeftest(lmout, vcov = vcovCluster(lmout, cluster = d$idnum))

# 
estmean <- na.omit(coef(lmout))
estvar  <- vcovCluster(lmout, cluster = d$idnum)

### INTERACTED MODELS
# get terms we need for effect
## use names in estvar
terms <- colnames(as.data.frame(estvar))[9:13] # Interaction Income and All Attributes , 5 items

# get positions
pos <- (1:length(estmean))[names(estmean) %in% terms ]
names(estmean)[pos]
# effect formula
g <- as.formula(paste("~",paste(paste("x",pos[1],"+",sep="")),
                      paste("1/5*(",
                            paste("x",pos[2:length(pos)],sep="",collapse="+")
                            ,")",sep="")
))
# ACME estimate
estmean[terms][1]+sum(estmean[terms][2:length(terms)])*1/5
# SE of ACME
deltamethod(g, estmean, estvar, ses=TRUE)



# estimate ACME using ZELIG individual intercepts model ("fixed effects model").

### Feb. 15th, 2016, Zelig wasn't loading the (1) graph nor the (2) Rgraphviz, so I loaded them individually
### It seemed that these two packages were not available for my R version on CRAN. They were available for latest R from source, though
### source("https://bioconductor.org/biocLite.R")
### biocLite("graph")
### source("https://bioconductor.org/biocLite.R")
### biocLite("Rgraphviz")

# install.packages("devtools")
# library(devtools)
# install_github("IQSS/Zelig")
library(Zelig)

logitgee.cj = zelig(selected ~ income.n,
                           #at.run*income.n + at.asso*income.n + at.press*income.n + at.presaut*income.n + at.vote*income.n +
                           #at.run*polknow + at.asso*polknow + at.press*polknow + at.presaut*polknow + at.vote*polknow,
                   model = "logit.gee", 
                   id = "idnum", 
                   cite = FALSE,
                   data = d)

## income plot
income.high <- setx(logitgee.cj, at.run*income.n = quantile(d$income.n, prob = 0.75))
income.low <- setx(logitgee.cj, income.n = quantile(d$income.n, prob = 0.25))



###############################################
# LIST 
###############################################
cat("\014")
rm(list=ls())

# Load Data
load( "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_list.RData") # Load data


# Histogram for Item Count
## Create a factor variable to use in the plot
dat$treatment.f = factor(dat$treatment, levels = c(0,1), labels=c("Control", "Treatment"))

library(ggplot2)
# Plot
ggplot(dat, aes(x=ycount)) + 
        geom_histogram(data=subset(dat, ycount>3), fill="red", binwidth=.5) +
        geom_histogram(data=subset(dat, ycount<=3), fill="forestgreen", binwidth=.5) +
        facet_grid(.~ treatment.f) + 
        xlab("Items") + 
        ylab("Item Count") +
        theme_bw()

# Histogram for Direct Question
library(ggplot2)
# Plot

ggplot.labels1 <- data.frame(
        time = c(1, 2), 
        value = c(1000, 300), 
        label = c(table(dat$directquestion.f)[1], table(dat$directquestion.f)[2]), 
        type = c("NA*", "MVH")
)

ggplot(dat[!is.na(dat$directquestion.f), ], aes(x=directquestion.f)) + geom_bar() +
        xlab("Would you be willing to accept money from a candidate for your vote?") + 
        ylab("Frequency") +
        geom_text(data = ggplot.labels1, aes(x = time, y = value, label = label), colour = "forestgreen") +
        theme_bw()


# Balance Plots PENDING

par(mfrow=c(3,2))

qqplot(jitter(tdat$age, amount=0.4), jitter(cdat$age, amount=0.4), xlab = "Treatment", ylab="Control", main="Age")
#dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Proposal_CESPS/PreTest/qq1B.pdf", width = 5, height = 5)
#

tdat$repdem <- as.numeric(tdat$repdem)
cdat$repdem <- as.numeric(cdat$repdem)
qqplot(jitter(tdat$repdem, amount=0.4), jitter(cdat$repdem, amount=0.4), xlab = "Treatment", ylab = "Control", main="Republican-Democrat")
#dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Proposal_CESPS/PreTest/qq2B.pdf", width = 5, height = 5)
#
tdat$repdem <- factor(tdat$repdem, labels = c("Strong Democrat", "Weak Democrat", "Independent Democrat", "Independent Independent", "Independent Republican", "Weak Republican", "Strong Republican"
))
cdat$repdem <- factor(cdat$repdem, labels = c("Strong Democrat", "Weak Democrat", "Independent Democrat", "Independent Independent", "Independent Republican", "Weak Republican", "Strong Republican"
))

qqplot(jitter(tdat$libcon, amount=0.4), jitter(cdat$libcon, amount=0.4), xlab = "Treatment", ylab = "Control", main="Liberal-Conservative")
#dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Proposal_CESPS/PreTest/qq3B.pdf", width = 5, height = 5)
#

qqplot(jitter(tdat$demsupp, amount=0.4), jitter(cdat$demsupp, amount=0.4), xlab = "Treatment", ylab= "Control", main="Democratic Support")
#dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Proposal_CESPS/PreTest/qq4B.pdf", width = 5, height = 5)
#

qqplot(jitter(tdat$polknow, amount=0.4), jitter(cdat$polknow, amount=0.4), xlab = "Treatment", ylab= "Control", main="Political Knowledge")
#dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Proposal_CESPS/PreTest/qq5B.pdf", width = 5, height = 5)
#


dat$male <- factor(dat$male, labels = c("Female", "Male", "NsDk"))
#cdat$male <- factor(cdat$male, labels = c("Female", "Male"))
#tdat$male <- factor(tdat$male, labels = c("Female", "Male", "NsDk"))
#counts1B <- table(tdat$male)
#counts2B <- table(cdat$male)
counts3B <- table(dat$male, dat$treat)
barplot(counts3B, ylab="Subjects (N)", main = "Gender"#, legend =rownames(counts3B)
)

#dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Proposal_CESPS/PreTest/b1B.pdf", width = 5, height = 5)
#
dat$male <- as.numeric(dat$male)
tdat$male <- as.numeric(tdat$male)
cdat$male <- as.numeric(cdat$male)
#barplot(counts2B, ylab="Subjects (N)", main = "Gender (control)")
#dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Proposal_CESPS/PreTest/b2B.pdf", width = 5, height = 5)
#

dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Proposal_CESPS/PreTest/balance_plots.pdf", width = 10, height = 10)
dev.off()
dev.off()


######################################################
# Analyses
######################################################
cat("\014")
rm(list=ls())

# Load Data
load( "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_list.RData") # Load data



# Difference in means 

##install.packages("list")
library(list)
dif.means <- ictreg(ycount ~ 1, data = dat, treat = "treatment", J=3, method = "ml")
sum.dif.means <- summary(dif.means, n.draws = 100000) # quasi-Bayesian approximation based predictions
summary(sum.dif.means)



# LOGISTIC MODEL

# Direct question


### Apr. 2nd, 2016, Zelig wasn't loading the (1) graph nor the (2) Rgraphviz, so I loaded them individually
### It seemed that these two packages were not available for my R version on CRAN. They were available for latest R from source, though
### source("https://bioconductor.org/biocLite.R")
### biocLite("graph")
### source("https://bioconductor.org/biocLite.R")
### biocLite("Rgraphviz")



### Previous Working model:  #age.n + woman + socideo + partyid + reg + trustfed + income.n + educ.n + polknow + #ziplabforce + #zippercamincome + sizetimesincome + proplabforgovtwork, 


# install.packages("devtools")
# library(devtools)
# install_github("IQSS/Zelig")
library(Zelig)
# Load Data
load( "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_list.RData") # Load data
# working model
direct.q.zelig <-   zelig(directquestion ~ 
                                  #age.n + 
                                  woman + 
                                  socideo + 
                                  partyid + 
                                  reg + 
                                  trustfed + 
                                  income.n + 
                                  educ.n + 
                                  polknow, 
                          #ziplabforce + 
                          #zippercamincome + 
                          #sizetimesincome + 
                          #proplabforgovtwork,               
                          data = dat, 
                    model = "logit")


# first differences RATIO LABOR FORCE / GVT WORK 
x.high <- setx(direct.q.zelig, proplabforgovtwork = quantile(dat$proplabforgovtwork, prob = 0.75))
x.low <- setx(direct.q.zelig, proplabforgovtwork = quantile(dat$proplabforgovtwork, prob = 0.25))
set.seed(602)
s.out2 <- sim(direct.q.zelig, x = x.high, x1 = x.low)
summary(s.out2)
plot(s.out2)



# Multivariate Analysis of List Experiment
library(list)
# Load Data
load( "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_list.RData") # Load data
## Setting tolerance
options(scipen=999)
options(digits=2)

list <- ictreg(ycount ~ 
                      #age.n + 
                      woman + 
                      socideo + 
                      partyid + 
                      reg + 
                      trustfed + 
                      income.n + 
                      educ.n + 
                      polknow, 
                      #ziplabforce + 
                      #zippercamincome + 
                      #sizetimesincome + 
                      #proplabforgovtwork,               
              data = dat, 
              treat = "treatment", 
              J=3, 
              method = "ml", 
              maxIter = 200000)

summary(list, n.draws = 200000) # quasi-Bayesian approximation based predictions

## Contextual Economic Model: MODEL ycount ~ zipinequality + sizeofthepoor + income.n + sizetimesincome + proplabforgovtwork
list.2 <- ictreg(ycount ~ zipinequality:income.n + sizeofthepoor:income.n + income.n + proplabforgovtwork,
              data = dat, 
              treat = "treatment", 
              J=3, 
              method = "nls", 
              maxIter = 200000)

summary(list.2, n.draws = 200000) # quasi-Bayesian approximation based predictions


## Soc. Des. Bias comparing list (individual) and list.2 (structural)

######################################################
# Testing for overdispertion 
## overdispersion is a concern due to possible positive correlation among control items, then beta-binomial logistic regression may be used

over.disp.test <-glm(ycount ~
                             #age.n + 
                             woman + 
                             socideo + 
                             partyid + 
                             reg + 
                             trustfed + 
                             income.n + 
                             educ.n + 
                             polknow, 
                     #ziplabforce + 
                     #zippercamincome + 
                     #sizetimesincome + 
                     #proplabforgovtwork,               
                     data = dat, 
                     family = poisson)
summary(over.disp.test)
plot(over.disp.test)

require(AER)
dispersiontest(over.disp.test)

######################################################
#####################################################
# Design effects

#ycount <- dat$ycount # Extracting vectors
#treat <- dat$treat # Extracting vectors
#treat <- factor(treat, labels = c("F", "T")) # Recoding treat as factor
#treat <- as.logical(treat) # Recoding treat as logical vector

#design <- ict.test(ycount, treat, J=3, gms = TRUE, n.draws = 250000, alpha = 0.05, pi.table = TRUE)
#print(design) # no design effect

######################################################
# Average predictions

## Proportion of affirmative responses to the sensitive item ("proportions of liars estimates")
list.predicted <- predict.ictreg(list, se.fit = TRUE, interval= "confidence", avg = T, return.draws = T, level = .95)
title=100*(list.predicted$fit$fit)
title=round(title, digits = 1)
plot(list.predicted, main = title) # produces plots with estimated population proportions of respondents answering the sensitive item

## Individual predictions
### Individual posterior likelihoods of vote-selling
list.predicted.2B <- predict.ictreg(list, se.fit = TRUE, interval= "confidence", avg = F, return.draws = T)
list.predicted.2B$fit<-round(list.predicted.2B$fit, 2)
list.predicted.2B$se.fit<-round(list.predicted.2B$se.fit, 2)
indpred.p = data.frame(list.predicted.2B$fit, list.predicted.2B$se.fit, sign = as.numeric(list.predicted.2B$fit$lwr<=0))
names(indpred.p)[4] = "se.fit"
rownames(indpred.p) <- NULL



### Plot
library(ggplot2)
ggplot() + geom_pointrange(data=indpred.p, 
                           mapping =aes(
                                   x=1:nrow(indpred.p), 
                                   y=indpred.p$fit, 
                                   ymin=indpred.p$lwr, 
                                   ymax=indpred.p$upr, 
                                   colour = indpred.p$sign
                           ), 
                           size=0.25, 
                           alpha=.5) + 
        theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Observations") + 
        ylab("Probability of Vote-Selling") +
        guides(colour=FALSE) + 
        theme_bw()

######################################################
# Estimation of Social Desirability: Direct vs. Indidirect questions


direct.q <- glm(directquestion ~ 
                        #age.n + 
                        woman + 
                        socideo + 
                        partyid + 
                        reg + 
                        trustfed + 
                        income.n + 
                        educ.n + 
                        polknow, 
                #ziplabforce + 
                #zippercamincome + 
                #sizetimesincome + 
                #proplabforgovtwork,               
                data = dat, 
                family = binomial(link = "logit"))


avg.pred.social.desirability <- predict.ictreg(list, direct.glm = direct.q, se.fit = TRUE)


socdes.p = data.frame(avg.pred.social.desirability$fit, 
                      avg.pred.social.desirability$se.fit,
                      c(1:3),
                      sign= as.numeric(avg.pred.social.desirability$fit$lwr<=0))

socdes.p$c.1.3 = as.factor(socdes.p$c.1.3)
socdes.p$c.1.3 <- factor(socdes.p$c.1.3, labels = c("List", "Direct", "Soc. Des"))

library(ggplot2)
ggplot() + geom_pointrange(
        data=socdes.p,
        mapping=aes(
                x=socdes.p$c.1.3, 
                y=socdes.p$fit,
                ymin=socdes.p$upr,
                ymax=socdes.p$lwr,
                colour = socdes.p$sign),size = 0.8) + 
        theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("") + 
        ylab("Probability of Vote-Selling") + 
        guides(colour=FALSE) +
        theme_bw()

######################################################
# PREDICTIONS
######################################################

# 1 woman
# 2 socideo
# 3 partyid
# 4 reg
# 5 trustfed
# 6 income


######################################################
# 1 WOMAN
womanW <- womanM <- dat
womanW <- dat[which(dat$woman=="Woman"), ] 
womanM <- dat[which(dat$woman=="Man"), ] 

library(list)
avg.pred.womanW  <- predict.ictreg(list, newdata = womanW, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.womanM  <- predict.ictreg(list, newdata = womanM, avg = TRUE, se.fit = TRUE, interval = "confidence")


woman.p = data.frame(
        t(avg.pred.womanW$fit), 
        t(avg.pred.womanM$fit)
)

woman.p = data.frame(t(woman.p))
sign = as.numeric(woman.p$lwr<=0)
woman.p["sign"] <- sign

woman.p$gender = as.factor(c(1,0))
woman.p$gender <- factor(woman.p$gender, levels = c(1,0), labels = c("Woman", "Man"))


library(ggplot2)
ggplot() + 
        geom_pointrange(
                data=woman.p, 
                mapping=aes(
                        x=woman.p$gender, 
                        y=woman.p$fit, 
                        ymin=woman.p$lwr, 
                        ymax=woman.p$upr,
                        colour = woman.p$sign),size = 0.8) + 
        theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Gender") + 
        ylab("Probability of Vote-Selling") +
        guides(colour=FALSE) +
        theme_bw()

######################################################
# 2 socideo

load( "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_list.RData") # Load data
socideoVC <- socideoC <- socideoM <- socideoL <- socideoVL <- dat
socideoVL <- dat[which(dat$socideo == levels(dat$socideo)[1]),]
socideoL <- dat[which(dat$socideo == levels(dat$socideo)[2]),]
socideoM <- dat[which(dat$socideo == levels(dat$socideo)[3]),]
socideoC <- dat[which(dat$socideo == levels(dat$socideo)[4]),]
socideoVC <- dat[which(dat$socideo == levels(dat$socideo)[5]),]

library(list)
avg.pred.socideoVL <- predict.ictreg(list, newdata = socideoVL, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.socideoL <- predict.ictreg(list, newdata = socideoL, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.socideoM <- predict.ictreg(list, newdata = socideoM, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.socideoC <- predict.ictreg(list, newdata = socideoC, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.socideoVC <- predict.ictreg(list, newdata = socideoVC, avg = TRUE, se.fit = TRUE, interval = "confidence")

socideo.p = data.frame(
        t(avg.pred.socideoVL$fit), 
        t(avg.pred.socideoL$fit), 
        t(avg.pred.socideoM$fit), 
        t(avg.pred.socideoC$fit), 
        t(avg.pred.socideoVC$fit)
)

socideo.p = data.frame(t(socideo.p))
sign = as.numeric(socideo.p$lwr<=0)
socideo.p["sign"] <- sign

socideo.p$socioideo = as.factor(c(1:5))
socideo.p$socioideo <- factor(socideo.p$socioideo, levels = c(1:5), labels = c("Very Liberal", "Liberal", "Moderate", "Conservative", "Very Conservative"))

library(ggplot2)
ggplot() + 
        geom_pointrange(
                data=socideo.p, 
                mapping=aes(
                        x=socideo.p$socioideo, 
                        y=socideo.p$fit, 
                        ymin=socideo.p$lwr, 
                        ymax=socideo.p$upr,
                        colour = socideo.p$sign),size = 0.8) + 
        theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Social Ideology") + 
        ylab("Probability of Vote-Selling") +
        guides(colour=FALSE) +
        theme_bw()

######################################################
# 3 partyid

load( "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_list.RData") # Load data
partyidD <- partyidR <- partyidI <- partyidSE <- dat

partyidD <- dat[which(dat$partyid == levels(dat$partyid)[1]),]
partyidR <- dat[which(dat$partyid == levels(dat$partyid)[2]),]
partyidI <- dat[which(dat$partyid == levels(dat$partyid)[3]),]
partyidSE <- dat[which(dat$partyid == levels(dat$partyid)[4]),]

library(list)
avg.pred.partyidD  <- predict.ictreg(list, newdata = partyidD, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.partyidR  <- predict.ictreg(list, newdata = partyidR, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.partyidI  <- predict.ictreg(list, newdata = partyidI, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.partyidSE  <- predict.ictreg(list, newdata = partyidSE, avg = TRUE, se.fit = TRUE, interval = "confidence")

partyid.p = data.frame(
        t(avg.pred.partyidD$fit), 
        t(avg.pred.partyidR$fit), 
        t(avg.pred.partyidI$fit), 
        t(avg.pred.partyidSE$fit)
)

partyid.p = data.frame(t(partyid.p))
sign = as.numeric(partyid.p$lwr<=0)
partyid.p["sign"] <- sign

partyid.p$partyid = as.factor(c(1:4))
partyid.p$partyid <- factor(partyid.p$partyid, levels = c(1:4), labels = c("Democrat", "Republican", "Independent", "Something Else"))

library(ggplot2)
ggplot() + 
        geom_pointrange(
                data=partyid.p, 
                mapping=aes(
                        x=partyid.p$partyid, 
                        y=partyid.p$fit, 
                        ymin=partyid.p$lwr, 
                        ymax=partyid.p$upr,
                        colour = partyid.p$sign),size = 0.8) + 
        theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Party Id.") + 
        ylab("Probability of Vote-Selling") +
        guides(colour=FALSE) +
        theme_bw()

######################################################
# 4 reg

load( "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_list.RData") # Load data
regR <- regU <- dat

regR <- dat[which(dat$reg == levels(dat$reg)[1]),]
regU <- dat[which(dat$reg == levels(dat$reg)[2]),]

library(list)
avg.pred.regR  <- predict.ictreg(list, newdata = regR, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.regU  <- predict.ictreg(list, newdata = regU, avg = TRUE, se.fit = TRUE, interval = "confidence")

reg.p = data.frame(
        t(avg.pred.regR$fit), 
        t(avg.pred.regU$fit)
)

reg.p = data.frame(t(reg.p))
sign = as.numeric(reg.p$lwr<=0)
reg.p["sign"] <- sign

reg.p$registered = as.factor(c(1:2))
reg.p$registered <- factor(reg.p$registered, levels = c(1:2), labels = c("Registered", "Not Registered"))

library(ggplot2)
ggplot() + 
        geom_pointrange(
                data=reg.p, 
                mapping=aes(
                        x=reg.p$registered, 
                        y=reg.p$fit, 
                        ymin=reg.p$lwr, 
                        ymax=reg.p$upr,
                        colour = reg.p$sign),size = 0.8) + 
        theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Registered to Vote") + 
        ylab("Probability of Vote-Selling") +
        guides(colour=FALSE) +
        theme_bw()

######################################################
# 5 trustfed

load( "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_list.RData") # Load data
trustfed.NTAA <- trustfed.NVMT <- trustfed.I <- trustfed.FAOT <- trustfed.AGDOT <- dat

trustfed.NTAA = dat[which(dat$trustfed == levels(dat$trustfed)[1]),]
trustfed.NVMT = dat[which(dat$trustfed == levels(dat$trustfed)[2]),]
trustfed.I = dat[which(dat$trustfed == levels(dat$trustfed)[3]),]
trustfed.FAOT = dat[which(dat$trustfed == levels(dat$trustfed)[4]),]
trustfed.AGDOT= dat[which(dat$trustfed == levels(dat$trustfed)[5]),]

library(list)
avg.pred.trustfed.NTAA = predict.ictreg(list, newdata = trustfed.NTAA, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.trustfed.NVMT = predict.ictreg(list, newdata = trustfed.NVMT, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.trustfed.I = predict.ictreg(list, newdata = trustfed.I, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.trustfed.FAOT = predict.ictreg(list, newdata = trustfed.FAOT, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.trustfed.AGDOT= predict.ictreg(list, newdata = trustfed.AGDOT, avg = TRUE, se.fit = TRUE, interval = "confidence")

trustfed.p = data.frame(
        t(avg.pred.trustfed.NTAA$fit),
        t(avg.pred.trustfed.NVMT$fit),
        t(avg.pred.trustfed.I$fit),
        t(avg.pred.trustfed.FAOT$fit),
        t(avg.pred.trustfed.AGDOT$fit)
)

trustfed.p = data.frame(t(trustfed.p))
sign = as.numeric(trustfed.p$lwr<=0)
trustfed.p["sign"] <- sign

trustfed.p$trustfed = as.factor(c(1:5))
trustfed.p$trustfed <- factor(trustfed.p$trustfed, levels = c(1:5), labels = c("No Trust At All", "Not Very Much Trust", "Indifferent", "Fair Amount Of Trust", "A Great Deal Of Trust"))

library(ggplot2)
ggplot() + 
        geom_pointrange(
                data=trustfed.p, 
                mapping=aes(
                        x=trustfed.p$trustfed, 
                        y=trustfed.p$fit, 
                        ymin=trustfed.p$lwr, 
                        ymax=trustfed.p$upr,
                        colour = trustfed.p$sign),size = 0.8) + 
        theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Trust in the Federal Government in Washington, DC") + 
        ylab("Probability of Vote-Selling") +
        guides(colour=FALSE) +
        theme_bw()


######################################################
# 6 educ
load( "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_list.RData") # Load data
educ.SHS <- educ.HS <- educ.T <- educ.SC <- educ.AD <- educ.BD <- educ.GS <- dat

educ.SHS = dat[which(dat$educ == levels(dat$educ)[1]),]
educ.HS = dat[which(dat$educ == levels(dat$educ)[2]),]
educ.T = dat[which(dat$educ == levels(dat$educ)[3]),]
educ.SC = dat[which(dat$educ == levels(dat$educ)[4]),]
educ.AD = dat[which(dat$educ == levels(dat$educ)[5]),]
educ.BD = dat[which(dat$educ == levels(dat$educ)[6]),]
educ.GS = dat[which(dat$educ == levels(dat$educ)[7]),]

library(list)
avg.pred.educ.SHS = predict.ictreg(list, newdata = educ.SHS, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.educ.HS = predict.ictreg(list, newdata = educ.HS, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.educ.T = predict.ictreg(list, newdata = educ.T, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.educ.SC = predict.ictreg(list, newdata = educ.SC, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.educ.AD = predict.ictreg(list, newdata = educ.AD, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.educ.BD = predict.ictreg(list, newdata = educ.BD, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.educ.GS = predict.ictreg(list, newdata = educ.GS, avg = TRUE, se.fit = TRUE, interval = "confidence")

educ.p = data.frame(
        t(avg.pred.educ.SHS$fit), 
        t(avg.pred.educ.HS$fit), 
        t(avg.pred.educ.T$fit), 
        t(avg.pred.educ.SC$fit), 
        t(avg.pred.educ.AD$fit), 
        t(avg.pred.educ.BD$fit), 
        t(avg.pred.educ.GS$fit)
)

educ.p = data.frame(t(educ.p))
sign = as.numeric(educ.p$lwr<=0)
educ.p["sign"] <- sign

educ.p$education = as.factor(c(1:7))
educ.p$education <- factor(educ.p$education, levels = c(1:7), labels = c("Some High School", "High School Graduate", "Technical School", "Some College", "Associate Degree", "Bachelor's Degree", "Graduate School"	)
)

library(ggplot2)
ggplot() + 
        geom_pointrange(
                data=educ.p, 
                mapping=aes(
                        x=educ.p$education, 
                        y=educ.p$fit, 
                        ymin=educ.p$lwr, 
                        ymax=educ.p$upr,
                        colour = educ.p$sign),size = 0.8) + 
        theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Type of Education") + 
        ylab("Probability of Vote-Selling") +
        guides(colour=FALSE) +
        theme_bw()

######################################################
# 6 income.n PENDING!!





######################################################
#### CONTEXTUAL ECONOMIC MODEL

# model: zipinequality:income.n + sizeofthepoor:income.n + income.n + proplabforgovtwork

# 1 zipinequality:income.n
# 2 sizeofthepoor:income.n
# 3 income.n
# 4 proplabforgovtwork

######################################################
# 1 zipinequality:income.n
load( "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_list.RData") # Load data

inequality.L <- inequality.H <- dat

inequality.L = dat[which(dat$zipinequality >= quantile(dat$zipinequality, prob = 0.25, na.rm = T)),]
inequality.H = dat[which(dat$zipinequality >= quantile(dat$zipinequality, prob = 0.75, na.rm = T)),]

library(list)
avg.pred.inequality.L = predict.ictreg(list.2, newdata = inequality.L, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.inequality.H = predict.ictreg(list.2, newdata = inequality.H, avg = TRUE, se.fit = TRUE, interval = "confidence")

inequality.p = data.frame(
        t(avg.pred.inequality.L$fit), 
        t(avg.pred.inequality.H$fit)
        )

inequality.p = data.frame(t(inequality.p))
sign = as.numeric(inequality.p$lwr<=0)
inequality.p["sign"] <- sign

inequality.p$inequality = as.factor(c(1:2))
inequality.p$inequality <- factor(inequality.p$inequality, levels = c(1:2), labels = c(
        "Low", 
        "High"))

library(ggplot2)
ggplot() + geom_pointrange(
        data=inequality.p, 
        mapping=aes(
                x=inequality.p$inequality, 
                y=inequality.p$fit, 
                ymin=inequality.p$lwr, 
                ymax=inequality.p$upr,
                colour = inequality.p$sign), 
        size = 0.8) + 
        theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Zip-Inequality") +
        ylab("Probability of Vote-Selling") +
        ggtitle("Interaction between Individual Income \nand Inequality at the Zip Level") +
        guides(colour=FALSE) +
        theme_bw()

######################################################
# 2 sizeofthepoor:income.n
load( "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_list.RData") # Load data

sizeofthepoor.L <- sizeofthepoor.S <- dat

sizeofthepoor.L = dat[which(dat$sizeofthepoor >= quantile(dat$sizeofthepoor, prob = 0.75, na.rm = T)),]
sizeofthepoor.S = dat[which(dat$sizeofthepoor >= quantile(dat$sizeofthepoor, prob = 0.25, na.rm = T)),]

library(list)
avg.pred.sizeofthepoor.L = predict.ictreg(list.2, newdata = sizeofthepoor.L, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.sizeofthepoor.S = predict.ictreg(list.2, newdata = sizeofthepoor.S, avg = TRUE, se.fit = TRUE, interval = "confidence")

sizeofthepoor.p = data.frame(
        t(avg.pred.sizeofthepoor.L$fit), 
        t(avg.pred.sizeofthepoor.S$fit)
)

sizeofthepoor.p = data.frame(t(sizeofthepoor.p))
sign = as.numeric(sizeofthepoor.p$lwr<=0)
sizeofthepoor.p["sign"] <- sign

sizeofthepoor.p$sizeofthepoor = as.factor(c(1:2))
sizeofthepoor.p$sizeofthepoor <- factor(sizeofthepoor.p$sizeofthepoor, levels = c(1:2), labels = c("Large", "Small"))

library(ggplot2)
ggplot() + geom_pointrange(
        data=sizeofthepoor.p, 
        mapping=aes(
                x=sizeofthepoor.p$sizeofthepoor, 
                y=sizeofthepoor.p$fit, 
                ymin=sizeofthepoor.p$lwr, 
                ymax=sizeofthepoor.p$upr,
                colour = sizeofthepoor.p$sign), 
        size = 0.8) + 
        theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Size of the Poor") +
        ylab("Probability of Vote-Selling") +
        ggtitle("Interaction between Individual Income \nand Size of the Poor at the Zip Level") +
        guides(colour=FALSE) +
        theme_bw()


######################################################
# 3 income.n
load( "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_list.RData") # Load data

income.L <- income.H <- dat

income.L = dat[which(dat$income.n >= quantile(dat$income.n, prob = 0.25, na.rm = T)),]
income.H = dat[which(dat$income.n >= quantile(dat$income.n, prob = 0.75, na.rm = T)),]

library(list)
avg.pred.income.L = predict.ictreg(list.2, newdata = income.L, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.income.H = predict.ictreg(list.2, newdata = income.H, avg = TRUE, se.fit = TRUE, interval = "confidence")

income.p = data.frame(
        t(avg.pred.income.L$fit), 
        t(avg.pred.income.H$fit)
        )

income.p = data.frame(t(income.p))
sign = as.numeric(income.p$lwr<=0)
income.p["sign"] <- sign

income.p$income = as.factor(c(1:2))
income.p$income <- factor(income.p$income, levels = c(1:2), labels = c("Low", "High"))

library(ggplot2)
ggplot() + geom_pointrange(
        data=income.p, 
        mapping=aes(
                x=income.p$income, 
                y=income.p$fit, 
                ymin=income.p$lwr, 
                ymax=income.p$upr,
                colour = income.p$sign), 
        size = 0.8) + 
        theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Income") +
        ylab("Probability of Vote-Selling") +
        ggtitle("Individual Income Levels") +
        guides(colour=FALSE) +
        theme_bw()


######################################################
# 4 proplabforgovtwork
load( "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_list.RData") # Load data
govtwork.H <- govtwork.L <- dat

govtwork.H = dat[which(dat$proplabforgovtwork >= quantile(dat$proplabforgovtwork, prob = 0.75)),]
govtwork.L = dat[which(dat$proplabforgovtwork >= quantile(dat$proplabforgovtwork, prob = 0.25)),]

library(list)
avg.pred.govtwork.H = predict.ictreg(list.2, newdata = govtwork.H, avg = TRUE, se.fit = TRUE, interval = "confidence")
avg.pred.govtwork.L = predict.ictreg(list.2, newdata = govtwork.L, avg = TRUE, se.fit = TRUE, interval = "confidence")

govtwork.p = data.frame(
        t(avg.pred.govtwork.H$fit), 
        t(avg.pred.govtwork.L$fit)
        )

govtwork.p = data.frame(t(govtwork.p))
sign = as.numeric(govtwork.p$lwr<=0)
govtwork.p["sign"] <- sign

govtwork.p$govtwork = as.factor(c(1:2))
govtwork.p$govtwork <- factor(govtwork.p$govtwork, levels = c(1:2), labels = c("Large", "Small"))

library(ggplot2)
ggplot() + 
        geom_pointrange(
                data=govtwork.p, 
                mapping=aes(
                        x=govtwork.p$govtwork, 
                        y=govtwork.p$fit, 
                        ymin=govtwork.p$lwr, 
                        ymax=govtwork.p$upr,
                        colour = govtwork.p$sign),size = 0.8) + 
        theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Size of the Government at the ZIP Level") + 
        ggtitle("") +
        ylab("Probability of Vote-Selling") +
        guides(colour=FALSE) +
        theme_bw()


###############################################
# Direct CLIENTELISM question plot from LAPOP
###############################################
load("/Users/hectorbahamonde/RU/Term5/Experiments_Redlawsk/Experiment/Data/LAPOP/datLAPOP.rdata")
clientelism = datLAPOP$clien1
clientelism <- factor(clientelism, labels = c("Often", "Sometines", "Never"))
clientelism <- na.omit(clientelism)

ggplot(, aes(x=clientelism)) + 
        geom_histogram(binwidth=.5, fill = I("grey50"), colour="black") + 
        xlab("Clientelism") + 
        ylab("Subjects(N)") +
        theme(plot.margin=unit(c(1,1,1,1),"cm")) + 
        ggsave(file="/Users/hectorbahamonde/RU/research/Vote_Selling/Proposal_CESPS/PreTest/clien1.pdf", width = 5, height = 5)
dev.off()
dev.off()

###############################################
# Vote-selling Pricing Survey Plot
###############################################

load("/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/Qualtrics/dat_list.RData") # Load data

# building auxiliary dataset
pricing.d = na.omit(data.frame(dat$pricecheap,dat$priceexpensive))
colnames(pricing.d)[1] <- "Cheap"
colnames(pricing.d)[2] <- "Expensive"

# reshaping dataset for density plots
library(reshape2)
pricing.d<- melt(pricing.d)

# Plot
ggplot(pricing.d,aes(x=value, fill=variable)) + 
        geom_density(alpha=0.25) +
        xlab("Price for your vote") + 
        ylab("Density") +
        scale_fill_discrete("") + #Price for Your Vote
        theme_bw() +
        theme(legend.position="bottom", legend.direction="horizontal")


