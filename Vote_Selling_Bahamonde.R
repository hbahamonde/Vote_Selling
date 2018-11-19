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


## ---- barplot:data:control:treatment ----

# Load Data
load( "/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data

# Histogram for Item Count
## Create a factor variable to use in the plot
dat$treatment.f = factor(dat$treatment, levels = c(0,1), labels=c("Control", "Treatment"))


treat.cont.bar.plot.d = data.frame(
        ycount = c(
                c(table(factor(dat$treatment100, levels = c(0,1,2,3,4), labels=c("0", "1", "2", "3", "4")))),
                c(table(factor(dat$treatment500, levels = c(0,1,2,3,4), labels=c("0", "1", "2", "3", "4")))),
                c(table(factor(dat$control, levels = c(0,1,2,3), labels=c("0", "1", "2", "3"))))
        ),
        Condition = c(
                rep("Low Treatment ($ 100)", 5),
                rep("High Treatment ($ 500)", 5),
                rep("Control", 4)
        ),
        xcount = c(
                0,1,2,3,4,#
                0,1,2,3,4,#
                0,1,2,3#
        ),
        n.items = c(
                rep("NonSens", 4), "Sens",
                rep("NonSens", 4), "Sens",
                rep("NonSens", 4)
        )
)


# reorder factor var to have low treatment BEFORE high treatment.
treat.cont.bar.plot.d$Condition = factor(treat.cont.bar.plot.d$Condition,levels(treat.cont.bar.plot.d$Condition)[c(1,3,2)])


# calculate percentages
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(plyr)

treat.cont.bar.plot.d = ddply(treat.cont.bar.plot.d, .(Condition), transform, percent = ycount/sum(ycount) * 100)
treat.cont.bar.plot.d = ddply(treat.cont.bar.plot.d, .(Condition), transform, pos = (cumsum(ycount) - 0.5 * ycount))
treat.cont.bar.plot.d$label = paste0(sprintf("%.0f", treat.cont.bar.plot.d$percent), "%")


# Plot
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

barplot.descriptive.plot = ggplot(treat.cont.bar.plot.d, 
                                  aes(x=xcount, 
                                      y = ycount,
                                      fill=n.items)) + 
        geom_bar(stat = "identity") + 
        facet_grid(.~ Condition) + 
        xlab("Number of Items") + 
        ylab("Frequency") +
        theme_bw() +
        scale_fill_manual(values=c("forestgreen", "forestgreen")) +
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=7), 
              axis.title.y = element_text(size=7), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=7), 
              legend.title=element_text(size=7),
              plot.title = element_text(size=7),
              strip.text.x = element_text(size = 7),
              legend.position="none") +
        geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2)
## ----


## ---- barplot:figure:control:treatment
# use this to explain plot in the paper
barplot.descriptive.plot
barplot.descriptive.plot.note <- paste(
        "{\\bf Frequency and Percentages of Subjects Declaring How Many (if any) Illegal Things They Would Do}.",
        "\\\\\\hspace{\\textwidth}", 
        "{\\bf Note}: Notice that the X-axis denotes the number of items, not which ones.",
        "\n")
## ----



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
sum.dif.means <- summary(dif.means, n.draws = 400000) # quasi-Bayesian approximation based predictions

sens.est = as.numeric(as.character(summary(sum.dif.means))[1])
sens.se = as.numeric(as.character(summary(sum.dif.means))[2])

cont.est = as.numeric(as.character(summary(sum.dif.means))[3])
cont.se = as.numeric(as.character(summary(sum.dif.means))[4])

# compute Cis [sens]
sens.upper = sens.est + 1.96*sens.se
sens.lower = sens.est - 1.96*sens.se

# compute Cis [cont]
cont.upper = cont.est + 1.96*cont.se
cont.lower = cont.est - 1.96*cont.se

# construct DF
diff.means.plot.d = data.frame(
        Variable = c("Sensitive", "Control"),
        Estimates = c(sens.est, cont.est),
        upper = c(sens.upper,cont.upper),
        lower = c(sens.lower,cont.lower)
)



if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

ggplot(diff.means.plot.d, aes(
        x = Variable, 
        y = Estimates, 
        ymin = upper, 
        ymax = lower)) +
        geom_pointrange() + 
        geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
        coord_flip() + 
        xlab("") + 
        ylab("Coefficient") +
        #ggtitle("Predicting Vote Selling: Broken Democratic Dimensions")+
        guides(colour=FALSE) +
        theme(legend.position="none") + 
        theme_bw()




# ycount.control.high.d = data.frame(dat$ycount[!is.na(dat$treatment500)], dat$ycount[!is.na(dat$control)], all = TRUE)
# ycount.control.low.d = data.frame(dat$ycount[!is.na(dat$treatment100)], dat$ycount[!is.na(dat$control)], all.x = TRUE)
# t.test(dat$ycount[!is.na(dat$treatment500)], dat$ycount[!is.na(dat$control)], conf.level = 0.95, paired = FALSE, alternative = "two.sided")

###########################################################
# Multivariate Analysis of List Experiment: Covariates
###########################################################

## ---- list:analysis:data ----
# cat("\014")
# rm(list=ls())

# Load Data 
load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data

# customization of ictreg
options(scipen=999)
method = as.character("ml")
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

ci.level  = 0.95

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

# summary(list.low, n.draws = 200000) # quasi-Bayesian approximation based predictions

## this is to construct a table later
n.draws = 200000
coeffs.treat.list.low = as.data.frame(summary(list.low, n.draws = n.draws)["par.treat"])[1:10,]
se.treat.list.low = as.data.frame(summary(list.low, n.draws = n.draws)["se.treat"])[1:10,]
coeffs.cont.list.low = as.data.frame(summary(list.low, n.draws = n.draws)["par.control"])[1:10,]
se.cont.list.low = as.data.frame(summary(list.low, n.draws = n.draws)["se.control"])[1:10,]





## Individual predictions
### Individual posterior likelihoods of vote-selling
list.low.predicted.2B <- predict.ictreg(list.low, se.fit = TRUE, interval= "confidence", avg = F, return.draws = T, level = ci.level)
list.low.predicted.2B$fit<-round(list.low.predicted.2B$fit, 2)
list.low.predicted.2B$se.fit<-round(list.low.predicted.2B$se.fit, 2)
indpred.p.low = data.frame(
        list.low.predicted.2B$fit, 
        list.low.predicted.2B$se.fit, 
        Significance = as.numeric(ifelse(sign(list.low.predicted.2B$fit$lwr) == sign(list.low.predicted.2B$fit$upr), 1,0)))
indpred.p.low$Significance[indpred.p.low$Significance==1] <- "Yes"
indpred.p.low$Significance[indpred.p.low$Significance==0] <- "No"
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

# summary(list.high, n.draws = 200000) # quasi-Bayesian approximation based predictions

## this is to construct a table later
coeffs.treat.list.high = as.data.frame(summary(list.high, n.draws = n.draws)["par.treat"])[1:10,]
se.treat.list.high = as.data.frame(summary(list.high, n.draws = n.draws)["se.treat"])[1:10,]
coeffs.cont.list.high = as.data.frame(summary(list.high, n.draws = n.draws)["par.control"])[1:10,]
se.cont.list.high = as.data.frame(summary(list.high, n.draws = n.draws)["se.control"])[1:10,]


## Individual predictions
### Individual posterior likelihoods of vote-selling
list.high.predicted.2B <- predict.ictreg(list.high, se.fit = TRUE, interval= "confidence", avg = F, return.draws = T, level = ci.level)
list.high.predicted.2B$fit<-round(list.high.predicted.2B$fit, 2)
list.high.predicted.2B$se.fit<-round(list.high.predicted.2B$se.fit, 2)
indpred.p.high = data.frame(
        list.high.predicted.2B$fit, 
        list.high.predicted.2B$se.fit, 
        Significance = as.numeric(ifelse(sign(list.high.predicted.2B$fit$lwr) == sign(list.high.predicted.2B$fit$up), 1,0)))
indpred.p.high$Significance[indpred.p.high$Significance==1] <- "Yes"
indpred.p.high$Significance[indpred.p.high$Significance==0] <- "No"
names(indpred.p.high)[4] = "se.fit"
rownames(indpred.p.high) <- NULL
indpred.p.high.fit= indpred.p.high$fit



######################################################
# table



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
                                                            colour = Significance), 
                                                    size=0.25, 
                                                    alpha=.5) + 
        #theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Observations") + 
        ylab("Probability of Vote-Selling\n(Low Condition)") +
        #guides(colour=FALSE) + 
        theme_bw() +
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=7), 
              axis.title.y = element_text(size=7), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=7), 
              legend.title=element_text(size=7),
              plot.title = element_text(size=7),
              legend.position="bottom")

## High
ind.pred.high.cond.plot = ggplot() + geom_pointrange(data=indpred.p.high, 
                                                     mapping =aes(
                                                             x=1:nrow(indpred.p.high), 
                                                             y=indpred.p.high$fit, 
                                                             ymin=indpred.p.high$lwr, 
                                                             ymax=indpred.p.high$upr, 
                                                             colour = indpred.p.high$Significance), 
                                                     size=0.25, 
                                                     alpha=.5) + 
        #theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Observations") + 
        ylab("Probability of Vote-Selling\n(High Condition)") +
        #guides(colour=FALSE) + 
        theme_bw() +
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=7), 
              axis.title.y = element_text(size=7), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=7), 
              legend.title=element_text(size=7),
              plot.title = element_text(size=7),
              legend.position="bottom")

# computing the sample size of the list experiment (which also determines the sample size in the conjoint portion) for the paper
total.sample.size = formatC(c(nrow(indpred.p.high) + nrow(indpred.p.low)), format="d", big.mark=",")

## merging the two plots
# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2,gridExtra)



# To force GGplots to share same legend.
grid_arrange_shared_legend <- function(...) {
        require(ggplot2)
        require(gridExtra)
        plots <- list(...)
        g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
        lheight <- sum(legend$height)
        grid.arrange(
                do.call(arrangeGrob, lapply(plots, function(x)
                        x + theme(legend.position="none"))),
                legend,
                ncol = 1,
                heights = grid::unit.c(unit(1, "npc") - lheight, lheight))
}

#### multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}
## ---- 




## ---- list:analysis:individual:predictions:plot  ----
# plot
grid_arrange_shared_legend(
        ind.pred.low.cond.plot, 
        ind.pred.high.cond.plot,
        ncol = 1, nrow = 2)

individual.predictions.plot.note <- paste(
        "{\\bf Individual Estimated Probabilities of Vote-Selling}.",
        "\\\\\\hspace{\\textwidth}", 
        paste(paste(paste(paste("{\\bf Note}: Figure shows the individual probabilities of vote-selling (N = ", total.sample.size, ")",  sep = ""), sep = ""), "under the ``low'' and ``high'' conditions. After fitting the model in \\autoref{tab:regression}, and following the advice of \\textcite[]{Blair2012} and \\textcite[]{Imai2014a}, individual probabilities of vote-selling under the ``low'' and ``high'' conditions were estimated. A total of ", paste(length(dat.with.predict$'Probability of Vote Selling'[dat.with.predict$sign==1])), "estimations are significant (both conditions).", sep = " "), paste("The figure also shows", paste(ci.level*100,"\\%", sep = ""), "confidence intervals.", sep = " ")),
        "\n")
## ----






######################################################
## Estimation of Social Desirability: Direct vs. Indidirect questions


##############
# Direct: High Condition
##############

## ---- list:analysis:social:desirability:data
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

avg.pred.social.desirability.high <- predict.ictreg(list.high, direct.glm = direct.q.high, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.social.desirability.low <- predict.ictreg(list.low, direct.glm = direct.q.low, se.fit = TRUE, interval = "confidence", level = ci.level)


### DF for individual prediction: High Condition
socdes.p.high = data.frame(avg.pred.social.desirability.high$fit, 
                           avg.pred.social.desirability.high$se.fit,
                           c(1:3),
                           Significance = c(ifelse(sign(min(seq(avg.pred.social.desirability.high$fit$lwr[1], avg.pred.social.desirability.high$fit$upr[1], 0.01))) == sign(max(seq(avg.pred.social.desirability.high$fit$lwr[1], avg.pred.social.desirability.high$fit$upr[1], 0.01))), 1,0), ifelse(sign(min(seq(avg.pred.social.desirability.high$fit$lwr[2], avg.pred.social.desirability.high$fit$upr[2], 0.01))) == sign(max(seq(avg.pred.social.desirability.high$fit$lwr[2], avg.pred.social.desirability.high$fit$upr[2], 0.01))), 1,0), ifelse(sign(min(seq(avg.pred.social.desirability.high$fit$lwr[3], avg.pred.social.desirability.high$fit$upr[3], 0.01))) == sign(max(seq(avg.pred.social.desirability.high$fit$lwr[3], avg.pred.social.desirability.high$fit$upr[3], 0.01))), 1,0)),
                           Condition = rep("High ($500)"), 3)


socdes.p.high$c.1.3 = as.factor(socdes.p.high$c.1.3)
socdes.p.high$c.1.3 <- factor(socdes.p.high$c.1.3, labels = c("List", "Direct", "Soc. Des."))
socdes.p.high <- socdes.p.high[c("fit", "lwr", "upr", "Significance", "Condition", "c.1.3")]

### DF for individual prediction: Low Condition
socdes.p.low = data.frame(avg.pred.social.desirability.low$fit, 
                          avg.pred.social.desirability.low$se.fit,
                          c(1:3),
                          Significance = c(ifelse(sign(min(seq(avg.pred.social.desirability.low$fit$lwr[1], avg.pred.social.desirability.low$fit$upr[1], 0.01))) == sign(max(seq(avg.pred.social.desirability.low$fit$lwr[1], avg.pred.social.desirability.low$fit$upr[1], 0.01))), 1,0), ifelse(sign(min(seq(avg.pred.social.desirability.low$fit$lwr[2], avg.pred.social.desirability.low$fit$upr[2], 0.01))) == sign(max(seq(avg.pred.social.desirability.low$fit$lwr[2], avg.pred.social.desirability.low$fit$upr[2], 0.01))), 1,0), ifelse(sign(min(seq(avg.pred.social.desirability.low$fit$lwr[3], avg.pred.social.desirability.low$fit$upr[3], 0.01))) == sign(max(seq(avg.pred.social.desirability.low$fit$lwr[3], avg.pred.social.desirability.low$fit$upr[3], 0.01))), 1,0)),
                          Condition = rep("Low ($100)"), 3)

socdes.p.low$c.1.3 = as.factor(socdes.p.low$c.1.3)
socdes.p.low$c.1.3 <- factor(socdes.p.low$c.1.3, labels = c("List", "Direct", "Soc. Des."))
socdes.p.low <- socdes.p.low[c("fit", "lwr", "upr", "Significance", "Condition", "c.1.3")]

### Rbinding both DF's
socdes.p.high.low = rbind(socdes.p.high, socdes.p.low)
rownames(socdes.p.high.low) <- NULL
socdes.p.high.low$Significance <- factor(socdes.p.high.low$Significance,
                                         levels = c(0,1),
                                         labels = c("Non-Significant", "Significant"))


### Plot
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

soc.des.plot = ggplot(socdes.p.high.low, 
                      aes(c.1.3, fit, colour = Significance, shape = Condition)) + 
        theme_bw() +
        xlab("") + 
        ylab("Estimated Proportion") +
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.2) +
        geom_pointrange(aes(
                x = socdes.p.high.low$c.1.3,
                ymin = socdes.p.high.low$lwr, 
                ymax = socdes.p.high.low$upr), position = position_dodge(width = 0.25)) +
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=7), 
              axis.title.y = element_text(size=7), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=7), 
              legend.title=element_text(size=7),
              plot.title = element_text(size=7))

## ----


## ---- list:analysis:social:desirability:plot
### Plot
soc.des.plot
soc.des.plot.note <- paste(
        "{\\bf List Experiment Data: Declared and Predicted Vote-Sellers}.",
        "\\\\\\hspace{\\textwidth}", 
        "{\\bf Note}: The figure shows the proportion of declared and predicted vote-sellers, and its difference (``liars''). These estimations were obtained from the model specified in \\autoref{tab:regression}.", paste("The figure shows ", ci.level*100, "\\% confidence intervals.", sep = ""), "Since the vote-selling prices were set arbitrarily, the reason for two conditions (``high'' and ``low'') was to control for possible price elasticities. While there are some perceptible changes, they are not statistically significant. Consequently, these arbitrary decisions do not threaten the identification strategy.",
        "\\\\\\hspace{\\textwidth}",
        "\n")
## ----


###########################################################################
# Predicting Which Of These Dimensions Predict Likely Vote-Sellers
###########################################################################


# cat("\014")
# rm(list=ls())


# USE THE VECTOR WITH INDIVUDUAL PREDICTIONS: High Condition
ind.pred.social.desirability.high <- predict.ictreg(list.high, se.fit = TRUE, interval= "confidence", avg = F, return.draws = T, level = ci.level)

ind.pred.social.desirability.high$fit<-round(ind.pred.social.desirability.high$fit, 10)
ind.pred.social.desirability.high$se.fit<-round(ind.pred.social.desirability.high$se.fit, 10)
ind.pred.social.desirability.high.d = data.frame(
        ind.pred.social.desirability.high$fit, 
        ind.pred.social.desirability.high$se.fit, 
        sign = ifelse(sign(ind.pred.social.desirability.high$fit$lwr) == sign(ind.pred.social.desirability.high$fit$upr), 1, 0))
names(ind.pred.social.desirability.high.d)[4] = "se.fit"
rownames(ind.pred.social.desirability.high.d) <- NULL


# USE THE VECTOR WITH INDIVUDUAL PREDICTIONS: Low Condition
ind.pred.social.desirability.low <- predict.ictreg(list.low, se.fit = TRUE, interval= "confidence", avg = F, return.draws = T, level = ci.level)

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
cat("\014")
rm(list=ls())

## ---- conjoint:analysis:predicting:vote:selling:data
# load conjoint data
load("/Users/hectorbahamonde/RU/research/Vote_Selling/mergedconjoint.RData") # d

## excluding non-significative values
# d <- d[ which(d$sign==1), ] # optional

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


# change reference ctegories // Reference are //democratic//
d <- within(d, at.run <- relevel(at.run, "Citizens CAN run for office for the next two elections"))
d <- within(d, at.asso <- relevel(at.asso, "Citizens CAN associate with others and form groups"))
d <- within(d, at.press <- relevel(at.press, "Media CAN confront the Government"))
d <- within(d, at.presaut <- relevel(at.presaut, "President CANNOT rule without Congress"))
d <- within(d, at.vote <- relevel(at.vote, "Citizens CAN vote in the next two elections"))

d <- d[ which(d$selected==1), ] 

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
        variable = c(
                # acme.vs.1
                "Citizens CANNOT run for office for the next two elections",
                "Citizens CAN run for office for the next two elections",
                # acme.vs.5
                "Citizens CANNOT vote in the next two elections",
                "Citizens CAN vote in the next two elections",
                # acme.vs.2
                "Citizens CANNOT associate with others and form groups",
                "Citizens CAN associate with others and form groups",
                # acme.vs.3
                "Media CANNOT confront the Government",
                "Media CAN confront the Government",
                # acme.vs.4
                "President CAN rule without Congress",
                "President CANNOT rule without Congress"
        ),
        coefficients = as.numeric(c(
                # sellers
                acme.vs.1[2], 0, # run
                acme.vs.5[2], 0, # vote
                acme.vs.2[2], 0, # assoc
                acme.vs.3[2], 0, # media
                acme.vs.4[2], 0
        )
        ),
        se = as.numeric(c(
                # sellers
                acme.vs.1[4], 0, # run
                acme.vs.5[4], 0, # vote
                acme.vs.2[4], 0, # assoc
                acme.vs.3[4], 0, # media
                acme.vs.4[4], 0
                )
                )
        )



ZScore <- 1.96

acme.vs.d$upper <-acme.vs.d$coefficients + ZScore*acme.vs.d$se # 1.28
acme.vs.d$lower <-acme.vs.d$coefficients - ZScore*acme.vs.d$se # 1.28

acme.vs.d$Component = c(rep("Democratic", 2), rep("Liberal", 2), rep("Republican", 1) )

acme.vs.d <- acme.vs.d[ which(
        acme.vs.d$coefficients!=0.000000000), ]



# Plot
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)


predicting.vote.selling.plot = ggplot(acme.vs.d, aes(
        x = variable, 
        y = coefficients, 
        ymin = upper, 
        ymax = lower,
        colour = Component)) +
        geom_pointrange(size=0.25) + 
        facet_wrap(~ Component , ncol = 1, scales = "free_y") + 
        geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
        coord_flip() + 
        xlab("") + 
        ylab("Coefficient") +
        #ggtitle("Predicting Vote Selling: Broken Democratic Dimensions")+
        guides(colour=FALSE) +
        theme_bw() +
        theme(axis.text.y = element_text(size=6), 
              axis.text.x = element_text(size=6), 
              axis.title.y = element_text(size=6), 
              axis.title.x = element_text(size=6), 
              legend.text=element_text(size=6), 
              legend.title=element_text(size=6),
              plot.title = element_text(size=6),
              legend.position="none", 
              strip.text.x = element_text(size = 6))
## ----

## ---- conjoint:analysis:predicting:vote:selling:plot
predicting.vote.selling.plot
predicting.vote.selling.plot.note <- paste(
        "{\\bf Predicting Vote Selling: Broken Democratic Dimensions}.",
        "\\\\\\hspace{\\textwidth}", 
        paste(paste(paste("{\\bf Note}: Figure shows the estimated dimension associated with vote-selling. After estimating the individual propensities for vote-selling via the list experiment, those estimations became the dependent variable in the conjoint portion. The plot shows which of Dahl's dimensions are associated with vote-selling.", as.character(if (1.96==ZScore) {"The 95\\%"} else { print("Error:Check CI") }), "confidence intervals are included. As per \\autoref{fig:list:analysis:individual:predictions:plot}, only statistically significant estimations were included. N ="), format(length(resid(model.vs.1)),big.mark=",",scientific=FALSE)), ". Note that every individual performed five tasks, i.e. ``voted'' in five ``elections.''", sep=""), "\n")
## ----



###########################################################################
# Democratic Values of the American Public
###########################################################################
cat("\014")
rm(list=ls())


## ---- conjoint:democratic:values:american:public:data

# Load Data
load("/Users/hectorbahamonde/RU/research/Vote_Selling/mergedconjoint.RData") # d


## excluding non-significative values
# d <- d[ which(d$sign==1), ] # optional

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

ZScore <- 1.96


acme.d$upper <-acme.d$coefficients + ZScore*acme.d$se
acme.d$lower <-acme.d$coefficients - ZScore*acme.d$se
acme.d$variable = order(acme.d$variable)

acme.d$variable <- factor(acme.d$variable,
                          levels = c(1,2,3,4,5,6,7,8,9,10),ordered=TRUE,
                          labels =   c("Citizens CAN run for office for the next two elections", "Citizens CANNOT run for office for the next two elections", "Citizens CAN vote in the next two elections","Citizens CANNOT vote in the next two elections", "Citizens CAN associate with others and form groups", "Citizens CANNOT associate with others and form groups", "Media CAN confront the Government","Media CANNOT confront the Government","President CANNOT rule without Congress", "President CAN rule without Congress")
)


acme.d$variable = with(acme.d, factor(variable, levels = rev(levels(variable))))



acme.d$Component = c(rep("Democratic", 4), rep("Liberal", 4), rep("Republican", 2) )
acme.d <- acme.d[ which(
        acme.d$coefficients!=0.000000000), ]

# Plot
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)


conjoint.democratic.values.american.public.plot = ggplot(acme.d, aes(
        x = variable, 
        y = coefficients, 
        ymin = upper, 
        ymax = lower,colour=acme.d$Component)
) +
        geom_pointrange(size=0.25) + 
        geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
        coord_flip() + 
        facet_wrap( ~Component,  ncol = 1, scales = "free_y") + 
        xlab("") + 
        ylab("Coefficient") +
        #ggtitle("Democratic Values of the American Public")+
        guides(colour=FALSE) +
        theme_bw() + 
        theme(axis.text.y = element_text(size=6), 
              axis.text.x = element_text(size=6), 
              axis.title.y = element_text(size=6), 
              axis.title.x = element_text(size=6), 
              legend.text=element_text(size=6), 
              legend.title=element_text(size=6),
              plot.title = element_text(size=6),
              legend.position="none", 
              strip.text.x = element_text(size = 6))

## ----

## ---- conjoint.democratic.values.american.public.plot
conjoint.democratic.values.american.public.plot
conjoint.democratic.values.american.public.note <- paste(
        "{\\bf Democratic Values of the American Public: A Conjoint Experimental Approach}.",
        "\\\\\\hspace{\\textwidth}", 
        paste(paste("{\\bf Note}: Besides answering the list experiment, survey respondents also answered a conjoint experiment. Following the advice of \\cite{Hainmueller2014a}, the ACME for every democratic sub-dimension (as conceptualized by \\cite{Dahl1971,ODonnell1998,Luna2006}) was computed. The figure shows estimated preferences towards different democratic sub-dimensions. For instance, and regarding the ``democratic'' dimension, survey respondents systematically value having political rights in the next two elections as opposed to not have them. Similarly, and regarding the ``republican'' component, survey respondents systematically value the role of Congress (as opposed to a system where the President ``CAN'' rule without Congress).", as.character(if (1.96==ZScore) {"The 95\\%"} else { print("Error:Check CI") })
                    , "confidence intervals are included. All observations were included. N =", format(length(resid(model.1)),big.mark=",",scientific=FALSE)), ".", sep=""), "\n")
## ---- 




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

## ---- design:effects:data


# define parameters
j = 3
alpha = 0.05

# Design effects [High Treatment]
ycount.high <- dat.high$ycount
treatment.high <- dat.high$treatment
design.high <- ict.test(ycount.high, treatment.high, J=j, gms = TRUE, n.draws = n.draws, alpha =alpha, pi.table = TRUE)
# print(design.high) # no design effect # NULL= no design. P value is above ALpha, so I fail to reject "no design effect."

# Design effects [Low Treatment]
ycount.low <- dat.low$ycount
treatment.low <- dat.low$treatment
design.low <- ict.test(ycount.low, treatment.low, J=j, gms = TRUE, n.draws = n.draws, alpha =alpha, pi.table = TRUE)
# print(design.low) # no design effect # NULL= no design. P value is above ALpha, so I fail to reject "no design effect."


#design.low.est = do.call(rbind.data.frame, design.low)[,1][2:9]
#design.low.se = do.call(rbind.data.frame, design.low)[,2][2:9]

#design.high.est = do.call(rbind.data.frame, design.high)[,1][2:9]
#design.high.se = do.call(rbind.data.frame, design.high)[,2][2:9]

# design.high.pvalue = do.call(rbind.data.frame, design.high)[,2][1]
# design.low.pvalue = do.call(rbind.data.frame, design.low)[,1][1]
## ----







######################################################
# PREDICTIONS
######################################################


## ---- predictions:independent:variables:data ----
# 2 socideo (ok)

load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data


socideoVC <- socideoC <- socideoM <- socideoL <- socideoVL <- dat
socideoVL <- dat[which(dat$socideo == levels(dat$socideo)[1]),]
socideoL <- dat[which(dat$socideo == levels(dat$socideo)[2]),]
socideoM <- dat[which(dat$socideo == levels(dat$socideo)[3]),]
socideoC <- dat[which(dat$socideo == levels(dat$socideo)[4]),]
socideoVC <- dat[which(dat$socideo == levels(dat$socideo)[5]),]

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(list)



## low
avg.pred.socideoVL.low <- predict.ictreg(list.low, newdata = socideoVL, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.socideoL.low <- predict.ictreg(list.low, newdata = socideoL, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.socideoM.low <- predict.ictreg(list.low, newdata = socideoM, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.socideoC.low <- predict.ictreg(list.low, newdata = socideoC, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.socideoVC.low <- predict.ictreg(list.low, newdata = socideoVC, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)

socideo.p.low = data.frame(
        t(avg.pred.socideoVL.low$fit), 
        t(avg.pred.socideoL.low$fit), 
        t(avg.pred.socideoM.low$fit), 
        t(avg.pred.socideoC.low$fit), 
        t(avg.pred.socideoVC.low$fit)
)

socideo.p.low = data.frame(t(socideo.p.low))
Significance = as.numeric(ifelse(sign(socideo.p.low$lwr) == sign(socideo.p.low$upr), 1,0))
socideo.p.low["Significance"] <- Significance
socideo.p.low$Significance[socideo.p.low$Significance==1] <- "Yes"
socideo.p.low$Significance[socideo.p.low$Significance==0] <- "No"
socideo.p.low$socioideo = as.factor(c(1:5))
socideo.p.low$socioideo <- factor(socideo.p.low$socioideo, levels = c(1:5), labels = c("Very \n Liberal", "Liberal", "Moderate", "Conservative", "Very \n Conservative"))
socideo.p.low$'Experimental Condition' <- "Low"



## high
avg.pred.socideoVL.high <- predict.ictreg(list.high, newdata = socideoVL, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.socideoL.high <- predict.ictreg(list.high, newdata = socideoL, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.socideoM.high <- predict.ictreg(list.high, newdata = socideoM, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.socideoC.high <- predict.ictreg(list.high, newdata = socideoC, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.socideoVC.high <- predict.ictreg(list.high, newdata = socideoVC, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)

socideo.p.high = data.frame(
        t(avg.pred.socideoVL.high$fit), 
        t(avg.pred.socideoL.high$fit), 
        t(avg.pred.socideoM.high$fit), 
        t(avg.pred.socideoC.high$fit), 
        t(avg.pred.socideoVC.high$fit)
)

socideo.p.high = data.frame(t(socideo.p.high))
Significance = as.numeric(ifelse(sign(socideo.p.high$lwr) == sign(socideo.p.high$upr), 1,0))
socideo.p.high["Significance"] <- Significance
socideo.p.high$Significance[socideo.p.high$Significance==1] <- "Yes"
socideo.p.high$Significance[socideo.p.high$Significance==0] <- "No"
socideo.p.high$socioideo = as.factor(c(1:5))
socideo.p.high$socioideo <- factor(socideo.p.high$socioideo, levels = c(1:5), labels = c("Very \n Liberal", "Liberal", "Moderate", "Conservative", "Very \n Conservative"))
socideo.p.high$'Experimental Condition' <- "High"


# Plot: High and Low
## merge both datasets
socideo.plot.d = data.frame(rbind(socideo.p.high, socideo.p.low))
names(socideo.plot.d)[names(socideo.plot.d) == "Experimental.Condition"] <- "Experimental Condition"

## plots
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

socio.plot = ggplot(socideo.plot.d, 
                    aes(sign, fit, colour = `Experimental Condition`)) + 
        theme_bw() +
        xlab("") + 
        ggtitle("Ideology") + 
        ylab("Estimated Proportion") +
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        geom_pointrange(aes(
                x = socideo.plot.d$socioideo,
                ymin = socideo.plot.d$lwr, 
                ymax = socideo.plot.d$upr), 
                position = position_dodge(width = 0.25)) +
        theme(axis.text.y = element_text(size=13), 
              axis.text.x = element_text(size=13), 
              axis.title.y = element_text(size=13), 
              axis.title.x = element_text(size=13), 
              legend.text=element_text(size=13), 
              legend.title=element_text(size=13),
              plot.title = element_text(size=14, face = "bold", hjust = 0.5),
              legend.position="bottom")



######################################################
# 3 partyid (ok)
load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data


partyidD <- partyidR <- partyidI <- partyidSE <- dat

partyidD <- dat[which(dat$partyid == levels(dat$partyid)[1]),]
partyidR <- dat[which(dat$partyid == levels(dat$partyid)[2]),]
partyidI <- dat[which(dat$partyid == levels(dat$partyid)[3]),]
partyidSE <- dat[which(dat$partyid == levels(dat$partyid)[4]),]

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(list)


## low
avg.pred.partyidD.low  <- predict.ictreg(list.low, newdata = partyidD, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.partyidR.low  <- predict.ictreg(list.low, newdata = partyidR, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.partyidI.low  <- predict.ictreg(list.low, newdata = partyidI, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.partyidSE.low  <- predict.ictreg(list.low, newdata = partyidSE, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)

partyid.p.low = data.frame(
        t(avg.pred.partyidD.low$fit), 
        t(avg.pred.partyidR.low$fit), 
        t(avg.pred.partyidI.low$fit), 
        t(avg.pred.partyidSE.low$fit)
)

partyid.p.low = data.frame(t(partyid.p.low))
Significance = as.numeric(ifelse(sign(partyid.p.low$lwr) == sign(partyid.p.low$upr), 1,0))
partyid.p.low["Significance"] <- Significance
partyid.p.low$Significance[partyid.p.low$Significance==1] <- "Yes"
partyid.p.low$Significance[partyid.p.low$Significance==0] <- "No"
partyid.p.low$partyid = as.factor(c(1:4))
partyid.p.low$partyid <- factor(partyid.p.low$partyid, levels = c(1:4), labels = c("Democrat", "Republican", "Independent", "Something\nElse"))
partyid.p.low$'Experimental Condition' <- "Low"


## high
avg.pred.partyidD.high  <- predict.ictreg(list.high, newdata = partyidD, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.partyidR.high  <- predict.ictreg(list.high, newdata = partyidR, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.partyidI.high  <- predict.ictreg(list.high, newdata = partyidI, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.partyidSE.high  <- predict.ictreg(list.high, newdata = partyidSE, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)

partyid.p.high = data.frame(
        t(avg.pred.partyidD.high$fit), 
        t(avg.pred.partyidR.high$fit), 
        t(avg.pred.partyidI.high$fit), 
        t(avg.pred.partyidSE.high$fit)
)

partyid.p.high = data.frame(t(partyid.p.high))
Significance = as.numeric(ifelse(sign(partyid.p.high$lwr) == sign(partyid.p.high$upr), 1,0))
partyid.p.high["Significance"] <- Significance
partyid.p.high$Significance[partyid.p.high$Significance==1] <- "Yes"
partyid.p.high$Significance[partyid.p.high$Significance==0] <- "No"
partyid.p.high$partyid = as.factor(c(1:4))
partyid.p.high$partyid <- factor(partyid.p.high$partyid, levels = c(1:4), labels = c("Democrat", "Republican", "Independent", "Something\nElse"))
partyid.p.high$'Experimental Condition' <- "High"


# Plot: High and Low
## merge both datasets
partyid.plot.d = data.frame(rbind(partyid.p.high, partyid.p.low))
names(partyid.plot.d)[names(partyid.plot.d) == "Experimental.Condition"] <- "Experimental Condition"


## plot

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)


partyid.plot = ggplot(partyid.plot.d, 
                      aes(sign, fit, colour = `Experimental Condition`)) + 
        theme_bw() +
        xlab("") + 
        ylab("Estimated Proportion") +
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        ggtitle("Party Identification") +
        geom_pointrange(aes(
                x = partyid.plot.d$partyid,
                ymin = partyid.plot.d$lwr, 
                ymax = partyid.plot.d$upr), 
                position = position_dodge(width = 0.25)) +
        theme(axis.text.y = element_text(size=13), 
              axis.text.x = element_text(size=13), 
              axis.title.y = element_text(size=13), 
              axis.title.x = element_text(size=13), 
              legend.text=element_text(size=13), 
              legend.title=element_text(size=13),
              plot.title = element_text(size=14, face = "bold", hjust = 0.5),
              legend.position="bottom")



######################################################
# 6 educ (ok)
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

## low

avg.pred.educ.SHS.low = predict.ictreg(list.low, newdata = educ.SHS, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.educ.HS.low = predict.ictreg(list.low, newdata = educ.HS, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.educ.T.low = predict.ictreg(list.low, newdata = educ.T, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.educ.SC.low = predict.ictreg(list.low, newdata = educ.SC, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.educ.AD.low = predict.ictreg(list.low, newdata = educ.AD, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.educ.BD.low = predict.ictreg(list.low, newdata = educ.BD, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.educ.GS.low = predict.ictreg(list.low, newdata = educ.GS, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)

educ.p.low = data.frame(
        t(avg.pred.educ.SHS.low$fit), 
        t(avg.pred.educ.HS.low$fit), 
        t(avg.pred.educ.T.low$fit), 
        t(avg.pred.educ.SC.low$fit), 
        t(avg.pred.educ.AD.low$fit), 
        t(avg.pred.educ.BD.low$fit), 
        t(avg.pred.educ.GS.low$fit)
)

educ.p.low = data.frame(t(educ.p.low))
Significance = as.numeric(ifelse(sign(educ.p.low$lwr) == sign(educ.p.low$upr), 1,0))
educ.p.low["Significance"] <- Significance
educ.p.low$Significance[educ.p.low$Significance==1] <- "Yes"
educ.p.low$Significance[educ.p.low$Significance==0] <- "No"
educ.p.low$education = as.factor(c(1:7))
educ.p.low$education <- factor(educ.p.low$education, levels = c(1:7), labels = c("Some \n High  \n School", "High \n  School \n  Graduate", "Technical \n  School", "Some  \n College", "Associate \n  Degree", "Bachelor's \n  Degree", "Graduate \n  School")
)
educ.p.low$'Experimental Condition' <- "Low"


## high
avg.pred.educ.SHS.high = predict.ictreg(list.high, newdata = educ.SHS, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.educ.HS.high = predict.ictreg(list.high, newdata = educ.HS, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.educ.T.high = predict.ictreg(list.high, newdata = educ.T, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.educ.SC.high = predict.ictreg(list.high, newdata = educ.SC, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.educ.AD.high = predict.ictreg(list.high, newdata = educ.AD, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.educ.BD.high = predict.ictreg(list.high, newdata = educ.BD, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
avg.pred.educ.GS.high = predict.ictreg(list.high, newdata = educ.GS, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)

educ.p.high = data.frame(
        t(avg.pred.educ.SHS.high$fit), 
        t(avg.pred.educ.HS.high$fit), 
        t(avg.pred.educ.T.high$fit), 
        t(avg.pred.educ.SC.high$fit), 
        t(avg.pred.educ.AD.high$fit), 
        t(avg.pred.educ.BD.high$fit), 
        t(avg.pred.educ.GS.high$fit)
)

educ.p.high = data.frame(t(educ.p.high))
Significance = as.numeric(ifelse(sign(educ.p.high$lwr) == sign(educ.p.high$upr), 1,0))
educ.p.high["Significance"] <- Significance
educ.p.high$Significance[educ.p.high$Significance==1] <- "Yes"
educ.p.high$Significance[educ.p.high$Significance==0] <- "No"
educ.p.high$education = as.factor(c(1:7))
educ.p.high$education <- factor(educ.p.high$education, levels = c(1:7), labels = c("Some \n High  \n School", "High \n  School \n  Graduate", "Technical \n  School", "Some  \n College", "Associate \n  Degree", "Bachelor's \n  Degree", "Graduate \n  School")
)
educ.p.high$'Experimental Condition' <- "High"

# Plot: High and Low
## merge both datasets
educ.plot.d = data.frame(rbind(educ.p.high, educ.p.low))
names(educ.plot.d)[names(educ.plot.d) == "Experimental.Condition"] <- "Experimental Condition"


## plots
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)


educ.plot = ggplot(educ.plot.d, 
                   aes(sign, fit, colour = `Experimental Condition`)) + 
        theme_bw() +
        xlab("") + 
        ylab("Estimated Proportion") +
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        ggtitle("Education") +
        geom_pointrange(aes(
                x = educ.plot.d$education,
                ymin = educ.plot.d$lwr, 
                ymax = educ.plot.d$upr), 
                position = position_dodge(width = 0.25)) +
        theme(axis.text.y = element_text(size=13), 
              axis.text.x = element_text(size=13), 
              axis.title.y = element_text(size=13), 
              axis.title.x = element_text(size=13), 
              legend.text=element_text(size=13), 
              legend.title=element_text(size=13),
              plot.title = element_text(size=14, face = "bold", hjust = 0.5),
              legend.position="bottom")


######################################################
# 6 income.n (ok)

load("/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data

income.1 <- income.2 <- income.3 <- income.4 <- income.5 <- income.6 <- income.7 <- income.8 <- income.9 <- income.10 <- income.11 <- income.12 <- income.13 <- income.14 <- dat

income.1  = dat[which(dat$income.n == 1),]
income.2  = dat[which(dat$income.n == 2),]
income.3  = dat[which(dat$income.n == 3),]
income.4  = dat[which(dat$income.n == 4),]
income.5  = dat[which(dat$income.n == 5),]
income.6  = dat[which(dat$income.n == 6),]
income.7  = dat[which(dat$income.n == 7),]
income.8  = dat[which(dat$income.n == 8),]
income.9  = dat[which(dat$income.n == 9),]
income.10 = dat[which(dat$income.n == 10),] 
income.11 = dat[which(dat$income.n == 11),] 
income.12 = dat[which(dat$income.n == 12),] 
income.13 = dat[which(dat$income.n == 13),] 
income.14 = dat[which(dat$income.n == 14),] 


if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(list)


## low
income.low.1  = predict.ictreg(list.low, newdata = income.1, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.low.2  = predict.ictreg(list.low, newdata = income.2, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.low.3  = predict.ictreg(list.low, newdata = income.3, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.low.4  = predict.ictreg(list.low, newdata = income.4, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.low.5  = predict.ictreg(list.low, newdata = income.5, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.low.6  = predict.ictreg(list.low, newdata = income.6, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.low.7  = predict.ictreg(list.low, newdata = income.7, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.low.8  = predict.ictreg(list.low, newdata = income.8, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.low.9  = predict.ictreg(list.low, newdata = income.9, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.low.10 = predict.ictreg(list.low, newdata = income.10, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level) 
income.low.11 = predict.ictreg(list.low, newdata = income.11, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level) 
income.low.12 = predict.ictreg(list.low, newdata = income.12, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level) 
income.low.13 = predict.ictreg(list.low, newdata = income.13, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level) 
income.low.14 = predict.ictreg(list.low, newdata = income.14, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level) 


income.p.low = data.frame(
        t(income.low.1$fit), 
        t(income.low.2$fit), 
        t(income.low.3$fit), 
        t(income.low.4$fit), 
        t(income.low.5$fit), 
        t(income.low.6$fit), 
        t(income.low.7$fit), 
        t(income.low.8$fit), 
        t(income.low.9$fit), 
        t(income.low.10$fit), 
        t(income.low.11$fit), 
        t(income.low.12$fit), 
        t(income.low.13$fit), 
        t(income.low.14$fit)
)

income.p.low = data.frame(t(income.p.low))
Significance = as.numeric(ifelse(sign(income.p.low$lwr) == sign(income.p.low$upr), 1,0))
income.p.low["Significance"] <- Significance
income.p.low$Significance[income.p.low$Significance==1] <- "Yes"
income.p.low$Significance[income.p.low$Significance==0] <- "No"
income.p.low$income = as.factor(c(1:14))
income.p.low$income <- factor(income.p.low$income, levels = c(1:14), labels = c("Less\nthan\n$20,000", "$20,000\nto\n$24,999", "$25,000\nto\n$29,999", "$30,000\nto\n$34,999", "$35,000\nto\n$39,999", "$40,000\nto\n$49,999", "$50,000\nto\n$59,999", "$60,000\nto\n$74,999", "$75,000\nto\n$84,999", "$85,000\nto\n$99,999", "$100,00\n to\n $124,999", "$125,00\n to\n $149,999", "$150,00\n to\n $174,999", "$175,000\nor\nmore")
)
income.p.low$'Experimental Condition' <- "Low"


## high
income.high.1  = predict.ictreg(list.high, newdata = income.1, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.high.2  = predict.ictreg(list.high, newdata = income.2, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.high.3  = predict.ictreg(list.high, newdata = income.3, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.high.4  = predict.ictreg(list.high, newdata = income.4, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.high.5  = predict.ictreg(list.high, newdata = income.5, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.high.6  = predict.ictreg(list.high, newdata = income.6, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.high.7  = predict.ictreg(list.high, newdata = income.7, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.high.8  = predict.ictreg(list.high, newdata = income.8, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.high.9  = predict.ictreg(list.high, newdata = income.9, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level)
income.high.10 = predict.ictreg(list.high, newdata = income.10, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level) 
income.high.11 = predict.ictreg(list.high, newdata = income.11, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level) 
income.high.12 = predict.ictreg(list.high, newdata = income.12, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level) 
income.high.13 = predict.ictreg(list.high, newdata = income.13, avg = TRUE, se.fit = TRUE, interval = "confidence", level = ci.level) 
income.high.14 = predict.ictreg(list.high, newdata = income.14, avg = TRUE, se.fit = TRUE, interval = "confidence" ,level = ci.level) 


income.p.high = data.frame(
        t(income.high.1$fit), 
        t(income.high.2$fit), 
        t(income.high.3$fit), 
        t(income.high.4$fit), 
        t(income.high.5$fit), 
        t(income.high.6$fit), 
        t(income.high.7$fit), 
        t(income.high.8$fit), 
        t(income.high.9$fit), 
        t(income.high.10$fit), 
        t(income.high.11$fit), 
        t(income.high.12$fit), 
        t(income.high.13$fit), 
        t(income.high.14$fit)
)

income.p.high = data.frame(t(income.p.high))
Significance = as.numeric(ifelse(sign(income.p.high$lwr) == sign(income.p.high$upr), 1,0))
income.p.high["Significance"] <- Significance
income.p.high$Significance[income.p.high$Significance==1] <- "Yes"
income.p.high$Significance[income.p.high$Significance==0] <- "No"
income.p.high$income = as.factor(c(1:14))
income.p.high$income <- factor(income.p.high$income, levels = c(1:14), labels = c("Less\nthan\n$20,000", "$20,000\nto\n$24,999", "$25,000\nto\n$29,999", "$30,000\nto\n$34,999", "$35,000\nto\n$39,999", "$40,000\nto\n$49,999", "$50,000\nto\n$59,999", "$60,000\nto\n$74,999", "$75,000\nto\n$84,999", "$85,000\nto\n$99,999", "$100,00\n to\n $124,999", "$125,00\n to\n $149,999", "$150,00\n to\n $174,999", "$175,000\nor\nmore")
)
income.p.high$'Experimental Condition' <- "High"


# Plot: High and Low
## merge both datasets
income.plot.d = data.frame(rbind(income.p.high, income.p.low))
names(income.plot.d)[names(income.plot.d) == "Experimental.Condition"] <- "Experimental Condition"


## plots
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

income.plot = ggplot(income.plot.d, 
                     aes(sign, fit, colour = `Experimental Condition`)) + 
        theme_bw() +
        xlab("") + 
        ylab("Estimated Proportion") +
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        geom_pointrange(aes(
                x = income.plot.d$income,
                ymin = income.plot.d$lwr, 
                ymax = income.plot.d$upr), 
                position = position_dodge(width = 0.25)) +
        ggtitle("Income") + 
        theme(axis.text.y = element_text(size=13), 
              axis.text.x = element_text(size=7, angle = 45, hjust=1), 
              axis.title.y = element_text(size=13), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=13), 
              legend.title=element_text(size=13),
              plot.title = element_text(size=14, face = "bold", hjust = 0.5),
              legend.position="bottom")


######################################################
#### MERGING ALL PLOTS

# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2,gridExtra)



# To force GGplots to share same legend.
grid_arrange_shared_legend <- function(...) {
        require(ggplot2)
        require(gridExtra)
        plots <- list(...)
        g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
        lheight <- sum(legend$height)
        grid.arrange(
                do.call(arrangeGrob, lapply(plots, function(x)
                        x + theme(legend.position="none"))),
                legend,
                ncol = 1,
                heights = grid::unit.c(unit(1, "npc") - lheight, lheight))
}

#### multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}
## ---- 

## ---- predictions:independent:variables:plot ----
grid_arrange_shared_legend(
        income.plot, 
        educ.plot,
        partyid.plot,
        socio.plot,
        ncol = 2, nrow = 2)

predictions.independent.variables.plot.note <- paste(
        "{\\bf List Experiment: Predicting Vote-Selling}.",
        "\\\\\\hspace{\\textwidth}", 
        paste(paste("{\\bf Note}: After fitting the model on the list experiment data (see \\autoref{tab:regression}), this figure shows  the predicted probabilities and their corresponding ", ci.level*100, "\\% confidence intervals for:", sep = ""), paste("income, education, party identification, and ideology. Since the vote-selling prices were set arbitrarily, the reason for two experimental conditions (``high'' and ``low'') was to control for possible price elasticities. While there are some perceptible changes, they are not statistically significant. Consequently, these arbitrary decisions do not threaten the identification strategy.")),
        "\n")
## ---- 


###############################################
# Direct CLIENTELISM question plot from LAPOP
###############################################

## ---- lapop:bar:chart:data ----
# cat("\014")
# rm(list=ls())
load("/Users/hectorbahamonde/RU/Term5/Experiments_Redlawsk/Experiment/Data/LAPOP/datLAPOP.rdata")
clientelism = datLAPOP$clien1
clientelism <- factor(clientelism, labels = c("Often", "Sometimes", "Never"))
clientelism <- na.omit(clientelism)

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)

clientelism = data.table(clientelism)

lapop.bar.chart.N = nrow(clientelism)

percentage.never = round(as.numeric(as.numeric(table(clientelism)["Never"]) * 100) / lapop.bar.chart.N, 1)
percentage.sometimes = round(as.numeric(as.numeric(table(clientelism)["Sometimes"]) * 100) / lapop.bar.chart.N, 1)
percentage.often = round(as.numeric(as.numeric(table(clientelism)["Often"]) * 100) / lapop.bar.chart.N, 1)



if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)
lapop.bar.chart.p = ggplot(clientelism, aes(clientelism)) + 
        geom_bar(stat="count", width = 0.5) + 
        theme_bw() +
        xlab("") + 
        ylab("Subjects (N)") +
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=7), 
              axis.title.y = element_text(size=7), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=7), 
              legend.title=element_text(size=7),
              plot.title = element_text(size=7),
              legend.position="bottom") +
        scale_x_discrete(labels=c(
                paste("Often (", paste(percentage.often, "%", sep = ""), ")", sep = ""),
                paste("Sometimes (", paste(percentage.sometimes, "%", sep = ""), ")", sep = ""),
                paste("Never (", paste(percentage.never, "%", sep = ""), ")", sep = "")
        ))
## ----

## ---- lapop:bar:chart:plot ----
### calling plot
lapop.bar.chart.p
### defining legend, title and notes.
lapop.bar.chart.p.note <- paste(
        "{\\bf Frequency of Clientelism in the United States (2010)}.",
        "\\\\\\hspace{\\textwidth}", 
        paste("{\\bf Note}: Figure shows the frequency of survey respondents, N = ", paste(lapop.bar.chart.N, ".", sep = ""), sep = ""),
        "\\\\\\hspace{\\textwidth}", 
        paste("{\\bf Source}: \\href{https://www.vanderbilt.edu/lapop/usa/2010_United_States_Questionnaire.pdf}{LAPOP}, 2010 wave for the United States. Question is \\texttt{clien1}: ``In recent years and thinking about election campaigns, has a candidate or someone from a political party offered you something, like a favor, food, or any other benefit or object in return for your vote or support? Has this happened often, sometimes, or never?''"),
        "\n")
## ----


###############################################
# Vote-selling Pricing Survey Plot
###############################################

## ---- pricing:experiment:data ----
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

price.plot = ggplot(pricing.d,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + 
        xlab("Price for your vote") + 
        ylab("Density") +
        scale_fill_discrete("") + #Price for Your Vote
        theme_bw() +
        theme(legend.position="bottom", legend.direction="horizontal")  +
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=7), 
              axis.title.y = element_text(size=7), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=7), 
              legend.title=element_text(size=7),
              plot.title = element_text(size=7),
              legend.position="bottom")

## getting intersecting price
pricing.d2 = data.frame(na.omit(data.frame(dat$pricecheap,dat$priceexpensive)))

# Getting the intersecting point.
## read data in the table
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(gdata)
options(scipen=999)

price.plot.d = ggplot_build(price.plot)
price.plot.d = data.frame(price.plot.d$data[[1]])
price.plot.d <- price.plot.d[order(price.plot.d$group),] # sort by price.plot.d


price.plot.d.1 = subset(price.plot.d, group == 1)
price.plot.d.1 <- price.plot.d.1[order(price.plot.d.1$x),] # sort by x

price.plot.d.2 = subset(price.plot.d, group == 2)
price.plot.d.2 <- price.plot.d.2[order(price.plot.d.2$x),] # sort by x

### https://github.com/andrewheiss/reconPlots
### https://www.andrewheiss.com/blog/2017/09/15/create-supply-and-demand-economics-curves-with-ggplot2/
### this package was used on Dec 11 2017
### It looks very new, and the arguments might not work in the future
### Today it was compiling good. 
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(devtools,Hmisc) # reconPlots needs, during installing process, Hmisc
install_github("andrewheiss/reconPlots")
library(reconPlots)


#### define x and y // package WONT work if these columns don't have same name
curve.1 = data.frame(price.plot.d.1$density, price.plot.d.1$x)
colnames(curve.1)[1] <- "y"
colnames(curve.1)[2] <- "x"

#### define x and y // package WONT work if these columns don't have same name
curve.2 = data.frame(price.plot.d.2$density, price.plot.d.2$x)
colnames(curve.2)[1] <- "y"
colnames(curve.2)[2] <- "x"

line_intersection <- curve_intersect(curve.1, curve.2)
## ---- 






## ---- pricing:experiment:plot ----
# calling the plot // need to save it to get the intersecting point.
price.plot + geom_vline(xintercept=line_intersection$x, colour = "red", linetype = "dashed", size = 0.5) 
price.plot.note <- paste(
        "{\\bf Pricing Experiment: Ideal Selling Price}.",
        "\\\\\\hspace{\\textwidth}", 
        paste(paste("{\\bf Note}: Subjects who answered ``yes'' to the direct question (N = ", nrow(na.omit(data.frame(dat$pricecheap,dat$priceexpensive))), ")", sep = ""), " were asked to price their votes via a pricing experiment (see \\autoref{fig:pricing:experiment}). This figure shows the empirical distributions of the ``too cheap'' and ``too expensive'' answers. The intersection of these two supply curves (the vertical dashed line) represents the estimated optimal selling price. The data suggest that the right price for one's vote is \\$", paste(round(line_intersection$x,0), ".", sep = ""), sep = ""),
        "\n")

## ---- 

round(line_intersection$x,0)
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



######################################################
# Descriptive Maps
######################################################
cat("\014")
rm(list=ls())

## ---- us:map:plot ----

# Load Data
load( "/Users/hectorbahamonde/RU/research/Vote_Selling/dat_list.RData") # Load data


## Map of Observations
### loading a package with ZIP codes and their respective Lat's and Long's.
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(zipcode,ggplot2,ggmap)

# install.packages("ggmap", type = "source")library(ggmap) # if it gives the following error ("Error: GeomRasterAnn was built with an incompatible version of ggproto"), install ggmap from source.



data(zipcode)
zipcode <- zipcode[c("zip", "latitude", "longitude")]
dat <- merge(dat,zipcode,by=c("zip"))
us <- c(left = -160, bottom = 15, right = -55, top = 50)
map <- get_stamenmap(us, zoom = 4, maptype = "toner-lite", scale = 2, format = "png")

levels(dat$partyid)[levels(dat$partyid)=="SomethingElse"] <- "Something Else"


ggmap(map) + geom_point(aes(
        x = longitude,
        colour=partyid,
        y = latitude), 
        alpha = .7,
        size = 0.8,
        shape = 1,
        data = dat) +
        xlab("Longitude") + 
        ylab("Latitude") +
        theme_bw() +
        labs(color='') +
        scale_colour_manual(values=c("blue", "red", "forestgreen", "cyan1")) +
        theme_bw() +
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=7), 
              axis.title.y = element_text(size=7), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=7), 
              legend.title=element_text(size=7),
              plot.title = element_text(size=7),
              legend.position="bottom")

## ----


######################################################
# Descriptive Maps: Vote Sellers
######################################################
cat("\014")
rm(list=ls())

## ---- us:map:vote:selling:data ----

# Load Data
load( "/Users/hectorbahamonde/RU/research/Vote_Selling/mergedconjoint_with_predicted_voteselling.RData") # Load data


## Map of Observations
### loading a package with ZIP codes and their respective Lat's and Long's.
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(zipcode,ggplot2,ggmap)




data(zipcode)
zipcode <- zipcode[c("zip", "latitude", "longitude")]
dat.with.predict <- merge(dat.with.predict,zipcode,by=c("zip"))
us <- c(left = -160, bottom = 15, right = -55, top = 50)
map <- get_stamenmap(us, zoom = 4, maptype = "toner-lite", scale = 2, format = "png") # maptype = "toner-lite"


colnames(dat.with.predict)[which(names(dat.with.predict) == "fit")] <- 'Probability of Vote Selling'


us.map.vote.selling.plot <- ggmap(map) + geom_point(aes(
        x = longitude,
        #colour=fit,
        y = latitude,
        colour = `Probability of Vote Selling`), 
        #colour = "red",
        alpha = .4,
        #size = 0.8,
        #shape = 21,
        data = dat.with.predict[dat.with.predict$sign==1,]) +
        xlab("Longitude") + 
        ylab("Latitude") +
        theme_bw() +
        #labs(color='') +
        theme_bw() + 
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=7), 
              axis.title.y = element_text(size=7), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=7), 
              legend.title=element_text(size=7),
              plot.title = element_text(size=7),
              legend.position="bottom") +
        scale_colour_gradient(low = "green", high = "red")
## ----

## ---- us:map:vote:selling:plot ----
us.map.vote.selling.plot
us.map.vote.selling.note <- paste(
        "{\\bf Mapping (Predicted) Vote-Sellers}.",
        "\\\\\\hspace{\\textwidth}", 
        paste("{\\bf Note}: Figure shows the geographical location (at the ZIP code level) of estimated vote-sellers. Using the estimations in \\autoref{tab:regression}, individual probabilities of vote-selling were obtained (see \\autoref{fig:list:analysis:individual:predictions:plot}). This map shows the geographical location of the estimations that are statistically significant only (N = ", paste(length(dat.with.predict$'Probability of Vote Selling'[dat.with.predict$sign==1])), ").", sep = ""),
        "\n")
## ----
