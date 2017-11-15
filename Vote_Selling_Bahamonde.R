############################## 
# Cleaning
##############################
cat("\014")
rm(list=ls())


# Load the data
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(foreign)

dat <- read.dta("/Users/hectorbahamonde/RU/research/Vote_Selling/data_list.dta")

# Data Cleaning
## Drop if Treatments are Missing

f = c("f_1_1_1",  "f_1_1_2", "f_1_1_3", "f_1_1_4", "f_1_1_5", "f_1_2_1", "f_1_2_2", "f_1_2_3", "f_1_2_4", "f_1_2_5", "f_2_1_1",  "f_2_1_2", "f_2_1_3", "f_2_1_4", "f_2_1_5", "f_2_2_1", "f_2_2_2", "f_2_2_3", "f_2_2_4", "f_2_2_5", "f_3_1_1", "f_3_1_2", "f_3_1_3", "f_3_1_4", "f_3_1_5", "f_3_2_1", "f_3_2_2", "f_3_2_3", "f_3_2_4", "f_3_2_5", "f_4_1_1", "f_4_1_2", "f_4_1_3", "f_4_1_4", "f_4_1_5", "f_4_2_1", "f_4_2_2", "f_4_2_3", "f_4_2_4", "f_4_2_5", "f_5_1_1", "f_5_1_2", "f_5_1_3", "f_5_1_4", "f_5_1_5", "f_5_2_1", "f_5_2_2", "f_5_2_3", "f_5_2_4", "f_5_2_5")

dat = dat[!with(dat,is.na(treatment100) & is.na(treatment500) & is.na(control) |
                  is.na(cj_1) | is.na(cj_2) | is.na(cj_3) | is.na(cj_4) | is.na(cj_5) ),]



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

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(car)

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
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(foreign)

zipdata <- read.dta("/Users/hectorbahamonde/RU/research/Vote_Selling/zipdata.dta") # import ZIP DTA DATA 
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

dat$age.n = as.numeric(dat$age) 
dat$income.n = as.numeric(dat$income)
dat$educ.n = as.numeric(dat$educ)

# drop missings THESE ARE THE COVARIATES I AM USING TO ESTIMATE THE LIST PART
sapply(dat, function(x) sum(is.na(x)))
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
dat = completeFun(dat, c("woman", "socideo", "partyid", "reg", "trustfed", "income.n", "educ.n", "polknow")) # variables



dat$proplabforgovtwork = dat$zipgovtworkers / dat$ziplabforc # Ratio Govt Workers/Labor Force ZIP level
dat$sizeofthepoor = rowSums(data.frame(dat$zipless10k,dat$zip1015k,dat$zip1525k)) / dat$ziplabforce # size of the poor
dat$zipinequality = as.numeric(dat$zipmeanincome - dat$zipmedianincome)



# generate the same IDNUM
idnum = data.frame(rep(1:nrow(dat)))
dat = data.frame(c(idnum, dat));colnames(dat)[1] <- "idnum"
#dat<-dat[!(dat$idnum=="245"),] # no se por qué chucha el 245 esta vacio por la reconchadesumadre 
dat$idnum = NULL


# Saving Data
save(dat, file = "/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # in paper's folder


###############################################
# LIST  Experiment DESCRIPTIVES
###############################################
cat("\014")
rm(list=ls())

# Load Data
load( "/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data

# Histogram for Item Count
## Create a factor variable to use in the plot
dat$treatment.f = factor(dat$treatment, levels = c(0,1), labels=c("Control", "Treatment"))


if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

# Plot
ggplot(dat, aes(x=ycount)) + 
  geom_histogram(data=subset(dat, ycount>3), fill="red", binwidth=.5) +
  geom_histogram(data=subset(dat, ycount<=3), fill="forestgreen", binwidth=.5) +
  facet_grid(.~ treatment.f) + 
  xlab("Items") + 
  ylab("Item Count") +
  theme_bw()

# Histogram for Direct Question
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

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
# Analyses LIST
######################################################
cat("\014")
rm(list=ls())

# Load Data
load( "/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data



# Difference in means 

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(list)

dif.means <- ictreg(ycount ~ 1, data = dat, treat = "treatment", J=3, method = "lm")
sum.dif.means <- summary(dif.means, n.draws = 100000) # quasi-Bayesian approximation based predictions
summary(sum.dif.means)



###########################################################
# Multivariate Analysis of List Experiment: Covariates
###########################################################
cat("\014")
rm(list=ls())
dev.off();dev.off();dev.off()

# Load Data 
load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data

# Define Formula
covariates.all = paste(
        "age.n", 
        "woman", 
        "socideo",
        "partyid",
        "reg",
        "trustfed",
        "income.n",
        "educ.n",
        "polknow",
        sep = " + ")

formula.list = formula(paste("ycount", covariates.all, sep = "~"))
formula.directquestion = formula(paste("directquestion", covariates.all, sep = "~"))


# customization of ictreg
method = as.character("lm")
maxIter = as.numeric(200000)
##############
# List Low Condition
##############


## subseting leaving only LOW treatment condition
dat.low <- dat[ which(dat$treatlow==1 | dat$treatment==0), ] 

# dropping rows with missing data
completeFun <- function(data, desiredCols) {
        completeVec <- complete.cases(data[, desiredCols])
        return(data[completeVec, ])
}

dat.low = completeFun(dat.low, 
                      c(
                              #"age.n", 
                              #"woman", 
                              "socideo", 
                              "partyid",
                              #"reg",
                              #"trustfed",
                              "income.n",
                              "educ.n"#,
                              #"polknow"
                              )
                )

## Setting tolerance
options(scipen=999)
options(digits=2)

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(list)

list.low <- ictreg(ycount ~ 
                           #age.n + 
                           #woman + 
                           socideo +
                           partyid +
                           #reg +
                           #trustfed +
                           income.n +
                           educ.n,# +
                           #polknow, 
                   data = dat.low, 
               treat = "treatment", 
               J=3, 
               method = method, 
               maxIter = maxIter)

summary(list.low, n.draws = 200000) # quasi-Bayesian approximation based predictions



## Individual predictions
### Individual posterior likelihoods of vote-selling
list.low.predicted.2B <- predict.ictreg(list.low, se.fit = TRUE, interval= "confidence", avg = F, return.draws = T)
list.low.predicted.2B$fit<-round(list.low.predicted.2B$fit, 2)
list.low.predicted.2B$se.fit<-round(list.low.predicted.2B$se.fit, 2)
indpred.p.low = data.frame(list.low.predicted.2B$fit, list.low.predicted.2B$se.fit, sign = as.numeric(list.low.predicted.2B$fit$lwr<=0))
names(indpred.p.low)[4] = "se.fit"
rownames(indpred.p.low) <- NULL
indpred.p.low.fit= indpred.p.low$fit


##############
# List High Condition
##############


## subseting leaving only LOW treatment condition
dat.high <- dat[ which(dat$treatlow==0 | dat$treatment==0), ] 

# dropping rows with missing data
completeFun <- function(data, desiredCols) {
        completeVec <- complete.cases(data[, desiredCols])
        return(data[completeVec, ])
}

dat.high = completeFun(dat.high, 
                      c(
                              #"age.n", 
                              #"woman", 
                              "socideo", 
                              "partyid",
                              #"reg",
                              #"trustfed",
                              "income.n",
                              "educ.n"#,
                              #"polknow"
                      )
                      )


## Setting tolerance
options(scipen=999)
options(digits=2)

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(list)

list.high <- ictreg(ycount ~ 
                            #age.n + 
                            #woman + 
                            socideo +
                            partyid +
                            #reg +
                            #trustfed +
                            income.n +
                            educ.n,# +
                    #polknow, 
                    data = dat.high, 
                    treat = "treatment", 
                    J=3, 
                    method = method, 
                    maxIter = maxIter)

summary(list.high, n.draws = 200000) # quasi-Bayesian approximation based predictions



## Individual predictions
### Individual posterior likelihoods of vote-selling
list.high.predicted.2B <- predict.ictreg(list.high, se.fit = TRUE, interval= "confidence", avg = F, return.draws = T)
list.high.predicted.2B$fit<-round(list.high.predicted.2B$fit, 2)
list.high.predicted.2B$se.fit<-round(list.high.predicted.2B$se.fit, 2)
indpred.p.high = data.frame(list.high.predicted.2B$fit, list.high.predicted.2B$se.fit, sign = as.numeric(list.high.predicted.2B$fit$lwr<=0))
names(indpred.p.high)[4] = "se.fit"
rownames(indpred.p.high) <- NULL
indpred.p.high.fit= indpred.p.high$fit


######################################################
# Average predictions
## Proportion of affirmative responses to the sensitive item ("proportions of liars estimates")
# list.predicted <- predict.ictreg(list, se.fit = TRUE, interval= "confidence", avg = T, return.draws = T, level = .95)
# title=100*(list.predicted$fit$fit)
# title=round(title, digits = 1)
# plot(list.predicted, main = title) # produces plots with estimated population proportions of respondents answering the sensitive item
######################################################

##############
# Plot Individual predictions (both High and Low conditions)
##############

# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2,grid,gridExtra)




## Low
ind.pred.low.cond.plot = ggplot() + geom_pointrange(data=indpred.p.low, 
                                                    mapping =aes(
                                                            x=1:nrow(indpred.p.low), 
                                                            y=indpred.p.low$fit, 
                                                            ymin=indpred.p.low$lwr, 
                                                            ymax=indpred.p.low$upr, 
                                                            colour = indpred.p.low$sign), 
                                                    size=0.25, 
                                                    alpha=.5) + 
        theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Observations") + 
        ylab("Probability of Vote-Selling (Low Condition)") +
        guides(colour=FALSE) + 
        theme_bw()

## High
ind.pred.high.cond.plot = ggplot() + geom_pointrange(data=indpred.p.high, 
                                                     mapping =aes(
                                                             x=1:nrow(indpred.p.high), 
                                                             y=indpred.p.high$fit, 
                                                             ymin=indpred.p.high$lwr, 
                                                             ymax=indpred.p.high$upr, 
                                                             colour = indpred.p.high$sign), 
                                                     size=0.25, 
                                                     alpha=.5) + 
        theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Observations") + 
        ylab("Probability of Vote-Selling (High Condition)") +
        guides(colour=FALSE) + 
        theme_bw()

## merging the two plots
grid.arrange(ind.pred.low.cond.plot, ind.pred.high.cond.plot, ncol = 1)


######################################################
## Estimation of Social Desirability: Direct vs. Indidirect questions


##############
# Direct: High Condition
##############


direct.q.high <- glm(directquestion ~ 
                             #age.n + 
                             #woman + 
                             socideo +
                             partyid +
                             #reg +
                             #trustfed +
                             income.n +
                             educ.n,# +
                     #polknow, 
                     data = dat.high, 
                family = binomial("logit"))


##############
# Direct: Low Condition
##############


direct.q.low <- glm(directquestion ~ 
                            #age.n + 
                            #woman + 
                            socideo +
                            partyid +
                            #reg +
                            #trustfed +
                            income.n +
                            educ.n,# +
                    #polknow, 
                    data = dat.low, 
                     family = binomial("logit"))

avg.pred.social.desirability.high <- predict.ictreg(list.high, direct.glm = direct.q.high, se.fit = TRUE, level = .95, interval = "confidence")
avg.pred.social.desirability.low <- predict.ictreg(list.low, direct.glm = direct.q.low, se.fit = TRUE, level = .95, interval = "confidence")


### DF for individual prediction: High Condition
socdes.p.high = data.frame(avg.pred.social.desirability.high$fit, 
                           avg.pred.social.desirability.high$se.fit,
                           c(1:3),
                           Significance = c(ifelse(sign(min(seq(avg.pred.social.desirability.high$fit$lwr[1], avg.pred.social.desirability.high$fit$upr[1], 0.01))) == sign(max(seq(avg.pred.social.desirability.high$fit$lwr[1], avg.pred.social.desirability.high$fit$upr[1], 0.01))), 1,0), ifelse(sign(min(seq(avg.pred.social.desirability.high$fit$lwr[2], avg.pred.social.desirability.high$fit$upr[2], 0.01))) == sign(max(seq(avg.pred.social.desirability.high$fit$lwr[2], avg.pred.social.desirability.high$fit$upr[2], 0.01))), 1,0), ifelse(sign(min(seq(avg.pred.social.desirability.high$fit$lwr[3], avg.pred.social.desirability.high$fit$upr[3], 0.01))) == sign(max(seq(avg.pred.social.desirability.high$fit$lwr[3], avg.pred.social.desirability.high$fit$upr[3], 0.01))), 1,0)),
                           Condition = rep("High"), 3)


socdes.p.high$c.1.3 = as.factor(socdes.p.high$c.1.3)
socdes.p.high$c.1.3 <- factor(socdes.p.high$c.1.3, labels = c("List", "Direct", "Soc. Des."))
socdes.p.high <- socdes.p.high[c("fit", "lwr", "upr", "Significance", "Condition", "c.1.3")]

### DF for individual prediction: Low Condition
socdes.p.low = data.frame(avg.pred.social.desirability.low$fit, 
                          avg.pred.social.desirability.low$se.fit,
                          c(1:3),
                          Significance = c(ifelse(sign(min(seq(avg.pred.social.desirability.low$fit$lwr[1], avg.pred.social.desirability.low$fit$upr[1], 0.01))) == sign(max(seq(avg.pred.social.desirability.low$fit$lwr[1], avg.pred.social.desirability.low$fit$upr[1], 0.01))), 1,0), ifelse(sign(min(seq(avg.pred.social.desirability.low$fit$lwr[2], avg.pred.social.desirability.low$fit$upr[2], 0.01))) == sign(max(seq(avg.pred.social.desirability.low$fit$lwr[2], avg.pred.social.desirability.low$fit$upr[2], 0.01))), 1,0), ifelse(sign(min(seq(avg.pred.social.desirability.low$fit$lwr[3], avg.pred.social.desirability.low$fit$upr[3], 0.01))) == sign(max(seq(avg.pred.social.desirability.low$fit$lwr[3], avg.pred.social.desirability.low$fit$upr[3], 0.01))), 1,0)),
                          Condition = rep("Low"), 3)

socdes.p.low$c.1.3 = as.factor(socdes.p.low$c.1.3)
socdes.p.low$c.1.3 <- factor(socdes.p.low$c.1.3, labels = c("List", "Direct", "Soc. Des."))
socdes.p.low <- socdes.p.low[c("fit", "lwr", "upr", "Significance", "Condition", "c.1.3")]

### Rbinding both DF's
socdes.p.high.low = rbind(socdes.p.high, socdes.p.low)
rownames(socdes.p.high.low) <- NULL
socdes.p.high.low$Significance <- factor(socdes.p.high.low$Significance,
       levels = c(0,1),
       labels = c("Non-Sign.", "Sign."))



### Plot
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

ggplot(socdes.p.high.low, 
       aes(c.1.3, fit, colour = Significance, shape = Condition)) + 
        theme_bw() +
        xlab("") + 
        ylab("Probability of Vote-Selling") +
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.2) +
        geom_pointrange(aes(
                x = socdes.p.high.low$c.1.3,
                ymin = socdes.p.high.low$lwr, 
                ymax = socdes.p.high.low$upr), position = position_dodge(width = 0.25))

       



###########################################################################
# Predicting Which Of These Dimensions Predict Likely Vote-Sellers
###########################################################################


# cat("\014")
# rm(list=ls())


# USE THE VECTOR WITH INDIVUDUAL PREDICTIONS: High Condition
ind.pred.social.desirability.high <- predict.ictreg(list.high, se.fit = TRUE, interval= "confidence", avg = F, return.draws = T)

ind.pred.social.desirability.high$fit<-round(ind.pred.social.desirability.high$fit, 10)
ind.pred.social.desirability.high$se.fit<-round(ind.pred.social.desirability.high$se.fit, 10)
ind.pred.social.desirability.high.d = data.frame(
        ind.pred.social.desirability.high$fit, 
        ind.pred.social.desirability.high$se.fit, 
        sign = ifelse(sign(ind.pred.social.desirability.high$fit$lwr) == sign(ind.pred.social.desirability.high$fit$upr), 1, 0))
names(ind.pred.social.desirability.high.d)[4] = "se.fit"
rownames(ind.pred.social.desirability.high.d) <- NULL


# USE THE VECTOR WITH INDIVUDUAL PREDICTIONS: Low Condition
ind.pred.social.desirability.low <- predict.ictreg(list.low, se.fit = TRUE, interval= "confidence", avg = F, return.draws = T)

ind.pred.social.desirability.low$fit<-round(ind.pred.social.desirability.low$fit, 10)
ind.pred.social.desirability.low$se.fit<-round(ind.pred.social.desirability.low$se.fit, 10)
ind.pred.social.desirability.low.d = data.frame(
        ind.pred.social.desirability.low$fit, 
        ind.pred.social.desirability.low$se.fit, 
        sign = ifelse(sign(ind.pred.social.desirability.low$fit$lwr) == sign(ind.pred.social.desirability.low$fit$upr), 1, 0))
names(ind.pred.social.desirability.low.d)[4] = "se.fit"
rownames(ind.pred.social.desirability.low.d) <- NULL

# cbind regular DFs (with the two conditions) with the predictions
dat.low.with.predict = data.frame(cbind(dat.low, ind.pred.social.desirability.low.d))
dat.high.with.predict = data.frame(cbind(dat.high, ind.pred.social.desirability.high.d))
dat.with.predict = data.frame(rbind(dat.low.with.predict, dat.high.with.predict))

# Saving Data
save(dat.with.predict, file = "/Users/hectorbahamonde/RU/research/Vote_Selling/mergedconjoint_with_predicted_voteselling.RData")


############################## 
# CONJOINT Experiment DATA CLEANING
##############################
cat("\014")
rm(list=ls())

# C
load("/Users/hectorbahamonde/RU/research/Vote_Selling/mergedconjoint_with_predicted_voteselling.RData")
c = dat.with.predict


# check for complete cases in CONJOINT and LIST treatments
c = subset(c, select = c("cj_1", "cj_2", "cj_3", "cj_4", "cj_5", "f_1_1_1",  "f_1_1_2", "f_1_1_3", "f_1_1_4", "f_1_1_5", "f_1_2_1", "f_1_2_2", "f_1_2_3", "f_1_2_4", "f_1_2_5", "f_2_1_1",  "f_2_1_2", "f_2_1_3", "f_2_1_4", "f_2_1_5", "f_2_2_1", "f_2_2_2", "f_2_2_3", "f_2_2_4", "f_2_2_5", "f_3_1_1", "f_3_1_2", "f_3_1_3", "f_3_1_4", "f_3_1_5", "f_3_2_1", "f_3_2_2", "f_3_2_3", "f_3_2_4", "f_3_2_5", "f_4_1_1", "f_4_1_2", "f_4_1_3", "f_4_1_4", "f_4_1_5", "f_4_2_1", "f_4_2_2", "f_4_2_3", "f_4_2_4", "f_4_2_5", "f_5_1_1", "f_5_1_2", "f_5_1_3", "f_5_1_4", "f_5_1_5", "f_5_2_1", "f_5_2_2", "f_5_2_3", "f_5_2_4", "f_5_2_5"))

# change names again
all_d = subset(c, select = c("cj_1", "cj_2", "cj_3", "cj_4", "cj_5"))
colnames(all_d)[which(names(all_d) == "cj_1")] <- "d1"
colnames(all_d)[which(names(all_d) == "cj_2")] <- "d2"
colnames(all_d)[which(names(all_d) == "cj_3")] <- "d3"
colnames(all_d)[which(names(all_d) == "cj_4")] <- "d4"
colnames(all_d)[which(names(all_d) == "cj_5")] <- "d5"

c$idnum = rep(1:nrow(c))
nrowc = nrow(c) # I will use this here to entry the number of VALID subjects I've got

# leave main dataframe with just the attributes and idnum
c = subset(c, select = -c(cj_1, cj_2, cj_3, cj_4, cj_5) )

# reshape dataset vertically
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(reshape)

c = melt(c, id="idnum")

# create code variable
c$variable = as.character(c$variable)

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(stringr)

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
# load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData")

e = dat.with.predict

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
fit.e = c(e$fit,e$fit,e$fit,e$fit,e$fit,e$fit,e$fit,e$fit,e$fit,e$fit)
lwr.e = c(e$lwr,e$lwr,e$lwr,e$lwr,e$lwr,e$lwr,e$lwr,e$lwr,e$lwr,e$lwr)
upr.e = c(e$upr,e$upr,e$upr,e$upr,e$upr,e$upr,e$upr,e$upr,e$upr,e$upr)
sign.e = c(e$sign,e$sign,e$sign,e$sign,e$sign,e$sign,e$sign,e$sign,e$sign,e$sign)

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
d$fit = rep(NA, nrowc*10)
d$lwr = rep(NA, nrowc*10)
d$upr = rep(NA, nrowc*10)
d$sign = rep(NA, nrowc*10)

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

outcome = data.frame(code.e,idnum.e, pair.e, candidate.e,all_d, woman.e, socideo.e, partyid.e, reg.e, trustfed.e, income.n.e, educ.n.e, polknow.e, fit.e, lwr.e, upr.e, sign.e)


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

for (i in code) {# fit
        d$fit[d$code==i] = outcome$fit[outcome$code==i]
}

for (i in code) {# lwr
        d$lwr[d$code==i] = outcome$lwr[outcome$code==i]
}

for (i in code) {# upr
        d$upr[d$code==i] = outcome$upr[outcome$code==i]
}

for (i in code) {# sign
        d$sign[d$code==i] = outcome$sign[outcome$code==i]
}

# selected
d$vote = ifelse(d$candidate == d$selected2, 1, 0)
d$selected = d$vote
d$selected2 = NULL
d$vote = NULL


# Saving Data
save(d, file = "/Users/hectorbahamonde/RU/research/Vote_Selling/mergedconjoint.RData")


######################################################################################
# Conjoint Analysis: Conjoint and List Data
######################################################################################


# load conjoint data
load("/Users/hectorbahamonde/RU/research/Vote_Selling/mergedconjoint.RData") # d

## excluding non-significative values
d <- d[ which(d$sign==1), ] # optional

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

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(lmtest,sandwich,msm)



# make outcome numeric
d$voteselling <- as.numeric(d$fit)

# make treatments factors
d$at.run = as.factor(d$at.run)
d$at.asso = as.factor(d$at.asso)
d$at.press = as.factor(d$at.press)
d$at.presaut = as.factor(d$at.presaut)
d$at.vote = as.factor(d$at.vote)


# change reference ctegories
d <- within(d, at.run <- relevel(at.run, ref = 2))
d <- within(d, at.asso <- relevel(at.asso, ref = 2))
d <- within(d, at.press <- relevel(at.press, ref = 2))
d <- within(d, at.presaut <- relevel(at.presaut, ref = 1))
d <- within(d, at.vote <- relevel(at.vote, ref = 2))

model.vs.1 = lm(voteselling ~ at.run, data=d)
model.vs.2 = lm(voteselling ~ at.asso, data=d)
model.vs.3 = lm(voteselling ~ at.press, data=d)
model.vs.4 = lm(voteselling ~ at.presaut, data=d)
model.vs.5 = lm(voteselling ~ at.vote, data=d)


d <- na.omit(d)
acme.vs.1 = coeftest(model.vs.1, vcov = vcovCluster(model.vs.1, cluster = d$idnum)) # run
acme.vs.2 = coeftest(model.vs.2, vcov = vcovCluster(model.vs.2, cluster = d$idnum)) # asso
acme.vs.3 = coeftest(model.vs.3, vcov = vcovCluster(model.vs.3, cluster = d$idnum)) # press
acme.vs.4 = coeftest(model.vs.4, vcov = vcovCluster(model.vs.4, cluster = d$idnum)) # pres aut
acme.vs.5 = coeftest(model.vs.5, vcov = vcovCluster(model.vs.5, cluster = d$idnum)) # vote

acme.vs.d <- data.frame(
        variable = seq(1:10),
        coefficients = as.numeric(c(
                acme.vs.1[2], 0, # run
                acme.vs.5[2], 0, # vote
                acme.vs.2[2], 0, # assoc
                acme.vs.3[2], 0, # media
                acme.vs.4[2], 0 # pres aut
        )
        ),
        se = as.numeric(c(
                acme.vs.1[4], 0, # run
                acme.vs.5[4], 0, # vote
                acme.vs.2[4], 0, # assoc
                acme.vs.3[4], 0, # media
                acme.vs.4[4], 0  # pres aut
        )
        )
)



acme.vs.d$upper <-acme.vs.d$coefficients + 1.28*acme.vs.d$se
acme.vs.d$lower <-acme.vs.d$coefficients - 1.28*acme.vs.d$se
acme.vs.d$variable = order(acme.vs.d$variable)


acme.vs.d$variable <- factor(acme.vs.d$variable,
                             levels = c(1,2,3,4,5,6,7,8,9,10),ordered=TRUE,
                             labels =   c("Democratic Component \n Citizens CAN run for office for the next two elections", "Citizens CANNOT run for office for the next two elections", "Citizens CAN vote in the next two elections","Citizens CANNOT vote in the next two elections", "Liberal Component \n Citizens CAN associate with others and form groups", "Citizens CANNOT associate with others and form groups", "Media CAN confront the Government","Media CANNOT confront the Government","Republican Component \n President CANNOT rule without Congress", "President CAN rule without Congress")
)


acme.vs.d$variable = with(acme.vs.d, factor(variable, levels = rev(levels(variable))))



# Plot
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

ggplot(acme.vs.d, aes(
        x = variable, 
        y = coefficients, 
        ymin = upper, 
        ymax = lower)
) +
        geom_pointrange() + 
        geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
        coord_flip() + 
        xlab("") + 
        ylab("Coefficient") +
        ggtitle("Predicting Vote Selling: Broken Democratic Dimensions")+
        guides(colour=FALSE) +
        theme(legend.position="none") + 
        theme_bw()







###########################################################################
# Democratic Values of the American Public
###########################################################################
# cat("\014")
# rm(list=ls())

# Load Data
load("/Users/hectorbahamonde/RU/research/Vote_Selling/mergedconjoint.RData") # d

d <- na.omit(d)

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

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(lmtest,sandwich,msm)



# make outcome numeric
d$selected <- as.numeric(d$selected)

# make treatments factors
d$at.run = as.factor(d$at.run)
d$at.asso = as.factor(d$at.asso)
d$at.press = as.factor(d$at.press)
d$at.presaut = as.factor(d$at.presaut)
d$at.vote = as.factor(d$at.vote)


# change reference ctegories
d <- within(d, at.run <- relevel(at.run, ref = 2))
d <- within(d, at.asso <- relevel(at.asso, ref = 2))
d <- within(d, at.press <- relevel(at.press, ref = 2))
d <- within(d, at.presaut <- relevel(at.presaut, ref = 1))
d <- within(d, at.vote <- relevel(at.vote, ref = 2))

model.1 = lm(selected ~ at.run, data=d)
model.2 = lm(selected ~ at.asso, data=d)
model.3 = lm(selected ~ at.press, data=d)
model.4 = lm(selected ~ at.presaut, data=d)
model.5 = lm(selected ~ at.vote, data=d)


acme.1 = coeftest(model.1, vcov = vcovCluster(model.1, cluster = d$idnum)) # run
acme.2 = coeftest(model.2, vcov = vcovCluster(model.2, cluster = d$idnum)) # asso
acme.3 = coeftest(model.3, vcov = vcovCluster(model.3, cluster = d$idnum)) # press
acme.4 = coeftest(model.4, vcov = vcovCluster(model.4, cluster = d$idnum)) # pres aut
acme.5 = coeftest(model.5, vcov = vcovCluster(model.5, cluster = d$idnum)) # vote

acme.d <- data.frame(
        variable = seq(1:10),
        coefficients = as.numeric(c(
                acme.1[2], 0, # run
                acme.5[2], 0, # vote
                acme.2[2], 0, # assoc
                acme.3[2], 0, # media
                acme.4[2], 0 # pres aut
        )
        ),
        se = as.numeric(c(
                acme.1[4], 0, # run
                acme.5[4], 0, # vote
                acme.2[4], 0, # assoc
                acme.3[4], 0, # media
                acme.4[4], 0  # pres aut
        )
        )
)

acme.d$upper <-acme.d$coefficients + 1.96*acme.d$se
acme.d$lower <-acme.d$coefficients - 1.96*acme.d$se
acme.d$variable = order(acme.d$variable)


acme.d$variable <- factor(acme.d$variable,
                          levels = c(1,2,3,4,5,6,7,8,9,10),ordered=TRUE,
                          labels =   c("Democratic Component \n Citizens CAN run for office for the next two elections", "Citizens CANNOT run for office for the next two elections", "Citizens CAN vote in the next two elections","Citizens CANNOT vote in the next two elections", "Liberal Component \n Citizens CAN associate with others and form groups", "Citizens CANNOT associate with others and form groups", "Media CAN confront the Government","Media CANNOT confront the Government","Republican Component \n President CANNOT rule without Congress", "President CAN rule without Congress")
)


acme.d$variable = with(acme.d, factor(variable, levels = rev(levels(variable))))



# Plot
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

ggplot(acme.d, aes(
        x = variable, 
        y = coefficients, 
        ymin = upper, 
        ymax = lower)
) +
        geom_pointrange() + 
        geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
        coord_flip() + 
        xlab("") + 
        ylab("Coefficient") +
        ggtitle("Democratic Values of the American Public")+
        guides(colour=FALSE) +
        theme(legend.position="none") + 
        theme_bw()





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

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(AER)

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
load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data

womanW <- womanM <- dat
womanW <- dat[which(dat$woman=="Woman"), ] 
womanM <- dat[which(dat$woman=="Man"), ] 

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(list)

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

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

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
load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data


socideoVC <- socideoC <- socideoM <- socideoL <- socideoVL <- dat
socideoVL <- dat[which(dat$socideo == levels(dat$socideo)[1]),]
socideoL <- dat[which(dat$socideo == levels(dat$socideo)[2]),]
socideoM <- dat[which(dat$socideo == levels(dat$socideo)[3]),]
socideoC <- dat[which(dat$socideo == levels(dat$socideo)[4]),]
socideoVC <- dat[which(dat$socideo == levels(dat$socideo)[5]),]

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(list)

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
socideo.p$socioideo <- factor(socideo.p$socioideo, levels = c(1:5), labels = c("Very \n Liberal", "Liberal", "Moderate", "Conservative", "Very \n Conservative"))

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

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
load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data


partyidD <- partyidR <- partyidI <- partyidSE <- dat

partyidD <- dat[which(dat$partyid == levels(dat$partyid)[1]),]
partyidR <- dat[which(dat$partyid == levels(dat$partyid)[2]),]
partyidI <- dat[which(dat$partyid == levels(dat$partyid)[3]),]
partyidSE <- dat[which(dat$partyid == levels(dat$partyid)[4]),]

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(list)

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

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)


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
load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data


regR <- regU <- dat

regR <- dat[which(dat$reg == levels(dat$reg)[1]),]
regU <- dat[which(dat$reg == levels(dat$reg)[2]),]

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(list)

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

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

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
load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data


trustfed.NTAA <- trustfed.NVMT <- trustfed.I <- trustfed.FAOT <- trustfed.AGDOT <- dat

trustfed.NTAA = dat[which(dat$trustfed == levels(dat$trustfed)[1]),]
trustfed.NVMT = dat[which(dat$trustfed == levels(dat$trustfed)[2]),]
trustfed.I = dat[which(dat$trustfed == levels(dat$trustfed)[3]),]
trustfed.FAOT = dat[which(dat$trustfed == levels(dat$trustfed)[4]),]
trustfed.AGDOT= dat[which(dat$trustfed == levels(dat$trustfed)[5]),]

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(list)

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
trustfed.p$trustfed <- factor(trustfed.p$trustfed, levels = c(1:5), labels = c("No Trust \n At All", "Not Very \n Much Trust", "Indifferent", "Fair Amount \n Of Trust", "A Great Deal \n Of Trust"))

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

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
load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data


educ.SHS <- educ.HS <- educ.T <- educ.SC <- educ.AD <- educ.BD <- educ.GS <- dat

educ.SHS = dat[which(dat$educ == levels(dat$educ)[1]),]
educ.HS = dat[which(dat$educ == levels(dat$educ)[2]),]
educ.T = dat[which(dat$educ == levels(dat$educ)[3]),]
educ.SC = dat[which(dat$educ == levels(dat$educ)[4]),]
educ.AD = dat[which(dat$educ == levels(dat$educ)[5]),]
educ.BD = dat[which(dat$educ == levels(dat$educ)[6]),]
educ.GS = dat[which(dat$educ == levels(dat$educ)[7]),]

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(list)

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
educ.p$education <- factor(educ.p$education, levels = c(1:7), labels = c("Some \n High  \n School", "High \n  School \n  Graduate", "Technical \n  School", "Some  \n College", "Associate \n  Degree", "Bachelor's \n  Degree", "Graduate \n  School")
)

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

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

load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data

inequality.L <- inequality.H <- dat

inequality.L = dat[which(dat$zipinequality >= quantile(dat$zipinequality, prob = 0.25, na.rm = T)),]
inequality.H = dat[which(dat$zipinequality >= quantile(dat$zipinequality, prob = 0.75, na.rm = T)),]

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(list)

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

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

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
load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data


sizeofthepoor.L <- sizeofthepoor.S <- dat

sizeofthepoor.L = dat[which(dat$sizeofthepoor >= quantile(dat$sizeofthepoor, prob = 0.75, na.rm = T)),]
sizeofthepoor.S = dat[which(dat$sizeofthepoor >= quantile(dat$sizeofthepoor, prob = 0.25, na.rm = T)),]

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(list)

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

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

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
load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data

income.L <- income.H <- dat

income.L = dat[which(dat$income.n >= quantile(dat$income.n, prob = 0.25, na.rm = T)),]
income.H = dat[which(dat$income.n >= quantile(dat$income.n, prob = 0.75, na.rm = T)),]

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(list)

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

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

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
load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data

govtwork.H <- govtwork.L <- dat

govtwork.H = dat[which(dat$proplabforgovtwork >= quantile(dat$proplabforgovtwork, na.rm = T, prob = 0.75)),]
govtwork.L = dat[which(dat$proplabforgovtwork >= quantile(dat$proplabforgovtwork, na.rm = T, prob = 0.25)),]

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(list)

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

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

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

load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data

# building auxiliary dataset
pricing.d = na.omit(data.frame(dat$pricecheap,dat$priceexpensive))
colnames(pricing.d)[1] <- "Cheap"
colnames(pricing.d)[2] <- "Expensive"

# reshaping dataset for density plots
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(reshape2)

pricing.d<- melt(pricing.d)

# Plot
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

ggplot(pricing.d,aes(x=value, fill=variable)) + 
  geom_density(alpha=0.25) +
  xlab("Price for your vote") + 
  ylab("Density") +
  scale_fill_discrete("") + #Price for Your Vote
  theme_bw() +
  theme(legend.position="bottom", legend.direction="horizontal")






############################## 
# CONJOINT Experiment DATA ANALYSES
##############################
cat("\014")
rm(list=ls())

# Load Data
load("/Users/hectorbahamonde/RU/research/Vote_Selling/mergedconjoint.RData") # Load data


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

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(lmtest,sandwich,msm)

# make outcome numeric
d$selected <- as.numeric(d$selected)

# make treatments factors
d$at.run = as.factor(d$at.run)
d$at.asso = as.factor(d$at.asso)
d$at.press = as.factor(d$at.press)
d$at.presaut = as.factor(d$at.presaut)
d$at.vote = as.factor(d$at.vote)


# change reference ctegories
d <- within(d, at.run <- relevel(at.run, ref = 2))
d <- within(d, at.asso <- relevel(at.asso, ref = 2))
d <- within(d, at.press <- relevel(at.press, ref = 2))
d <- within(d, at.presaut <- relevel(at.presaut, ref = 1))
d <- within(d, at.vote <- relevel(at.vote, ref = 2))

model.1 = lm(selected ~ at.run, data=d)
model.2 = lm(selected ~ at.asso, data=d)
model.3 = lm(selected ~ at.press, data=d)
model.4 = lm(selected ~ at.presaut, data=d)
model.5 = lm(selected ~ at.vote, data=d)


acme.1 = coeftest(model.1, vcov = vcovCluster(model.1, cluster = d$idnum))
acme.2 = coeftest(model.2, vcov = vcovCluster(model.2, cluster = d$idnum))
acme.3 = coeftest(model.3, vcov = vcovCluster(model.3, cluster = d$idnum))
acme.4 = coeftest(model.4, vcov = vcovCluster(model.4, cluster = d$idnum))
acme.5 = coeftest(model.5, vcov = vcovCluster(model.5, cluster = d$idnum))


acme.d <- data.frame(coefficients = c(acme.1[2], acme.2[2],acme.3[2],acme.4[2],acme.5[2]),
                     se = c(acme.1[4], acme.2[4],acme.3[4],acme.4[4],acme.5[4]),
                     variable = c(
                       "Citizens CAN run for office for the next two elections", 
                       "Citizens CAN associate with others and form groups",
                       "Media CAN confront the Government",
                       "President CANNOT rule without Congress",
                       "Citizens CAN vote in the next two elections"
                     )
)
acme.d$upper <-acme.d$coefficient + 1.96*acme.d$se
acme.d$lower <-acme.d$coefficient - 1.96*acme.d$se



acme.0 = data.frame(
  variable = c("Citizens CANNOT run for office for the next two elections", 
               "Citizens CANNOT associate with others and form groups",
               "Media CANNOT confront the Government",
               "President CAN rule without Congress",
               "Citizens CANNOT vote in the next two elections"),
  coefficients = c(rep(NA, times = 5)), 
  se = c(rep(NA, times = 5)),
  upper = c(rep(NA, times = 5)),
  lower = c(rep(NA, times = 5))
)

acme.d = rbind(acme.d,acme.0)

acme.d$attribute = c(
  "Right To Run", "Right to Associate", "Free Press", "President Autonomy", "Right to Vote",
  "Right To Run", "Right to Associate", "Free Press", "President Autonomy", "Right to Vote"
)




# Plot
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

ggplot() + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_pointrange(data=acme.d, 
                  mapping=aes(x=variable, 
                              y=coefficients, 
                              ymin=upper, 
                              ymax=lower),
                  #shape=22, 
                  fill = "WHITE") +
  coord_flip() + 
  xlab("") + 
  ylab("Coefficient") +
  guides(colour=FALSE) +
  theme(legend.position="none") + 
  theme_bw()

############
## Predicting Predicted Probabiltities of Vote Selling
############

load( "/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data

voteselling = data.frame(rep(indpred.p.fit, each = 10))
dat.combined = data.frame(voteselling, dat); colnames(dat.combined)[1] <- "voteselling"






