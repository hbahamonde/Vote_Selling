cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/RU/research/Vote_Selling/Data/Qualtrics")
install.packages("sandwich")
install.packages("optmatch")
install.packages("Matching", dependencies=TRUE)
install.packages("rgenoud")
install.packages("car")
install.packages("relogit")
install.packages("MatchIt", dependencies=TRUE)
install.packages("MASS")
install.packages("ggplot2")
install.packages("corrgram")
install.packages("stargazer")
install.packages("Zelig")
install.packages("reshape")
install.packages("simPH")
install.packages("plyr")
install.packages("grid")
install.packages("gridExtra")
install.packages("psych")
install.packages("list")
install.packages("lattice")
library(lattice)
library(list)
library(psych)
library(gridExtra)
library(datasets)
library(Zelig)
library(sandwich)
library(car)
library(MASS)
library(MatchIt)
library(MASS)
library(lattice)
library(foreign)
library(WhatIf)
library(stargazer)
library(grid)
library(ggplot2)

###############################################
# Loading the Data (pre-cleaned in STATA)
# load("/Users/hectorbahamonde/RU/Term5/Experiments_Redlawsk/Experiment/Data/Qualtrics/Vote_Selling.rdata")
# Generates a var called "treat" which indicates 1 for treated and 0 for control
# treat <- with(Vote_Selling,ifelse(listt != "NA",1, 0))
# treat <- as.data.frame(treat)
# treat[is.na(treat)] <- 0
# New data is called "dat"
# dat <- cbind(Vote_Selling, treat)
# 
# Generating y.count (count data for list experiment)
# dat[is.na(dat)] <- 0
# dat$y.count <- dat$listt + dat$listc
# dat[dat=="99"] <- NA
###############################################

# "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper"
load("/Users/hectorbahamonde/RU/research/Vote_Selling/Data/Qualtrics/dat.rdata")
###############################################

dat$male <- factor(dat$male, labels = c("F", "M"))
# dat$ethnicity <- factor(dat$ethnicity, labels = c("Whi", "Lat", "Bla", "AmIn", "Asi"))
# dat$religion <- factor(dat$religion, labels = c("Prot", "Cath", "Jew", "Orth", "Ath", "Agn"))
# dat$democrat <- factor(dat$democrat, labels = c("Rep", "Dem", "Ind", "NV"))
dat$treat <- factor(dat$treat, labels = c("control", "treatment"))

tdat <- subset(dat, treat=="treatment") # subsetting data
cdat <- subset(dat, treat=="control") # subsetting data
cdat$male <- factor(cdat$male, labels = c("F", "M")) # labeling data
tdat$male <- factor(tdat$male, labels = c("F", "M")) # labeling data
cdat$voteselling1 <- factor(cdat$voteselling1, labels = c("Didn't Sell", "Sold")) # labeling data
# dat$buying1 <- factor(dat$buying1, labels = c("100", "300", "500", "800", "1k", "1.5k", "2k"))
# dat$buying2 <- factor(dat$buying2, labels = c("100", "300", "500", "800", "1k", "1.5k", "2k"))

# Summary
table(dat$listc)
table(dat$listt)
summary(dat$listc)
summary(dat$listt)
(1.732-1.385)*100
# histogram(dat$listt) 
# histogram(dat$listc)

# Correlation between item counts
t.itemcount <- tdat$ycount[1:96]
c.itemcount <- cdat$ycount
itemcount <- data.frame(t.itemcount, c.itemcount)
corr(itemcount)

# Histogram for Item Count
ggplot(dat, aes(x=ycount)) + 
  geom_histogram(binwidth=.5, fill = I("grey50"), colour="black") + 
  facet_grid(treat ~ .) + 
  xlab("Items") + 
  theme(plot.margin=unit(c(1,1,1,1),"cm")) + 
  ggsave(file="/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/distribution_items.pdf", width = 5, height = 5)


# QQplots: describing the data WITH TITLE
qq1 <- qqplot(tdat$age, cdat$age, main= "Age", xlab = "Treatment", ylab="Control")
qq1
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/qq1.pdf", width = 5, height = 5)
dev.off()

qq2 <- qqplot(tdat$repdem, cdat$repdem, main ="Republican/Democrat", xlab = "Treatment", ylab = "Control")
qq2
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/qq2.pdf", width = 5, height = 5)
dev.off()

qq3 <- qqplot(tdat$libcon, cdat$libcon, main= "Liberal/Conservative", xlab = "Treatment", ylab = "Control")
qq3
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/qq3.pdf", width = 5, height = 5)
dev.off()

qq4 <- qqplot(tdat$demsupp, cdat$demsupp, main= "Democratic Support", xlab = "Treatment", ylab= "Control")
qq4
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/qq4.pdf", width = 5, height = 5)
dev.off()


# QQplots: describing the data WITHOUT TITLE
qq1B <- qqplot(tdat$age, cdat$age, xlab = "Treatment", ylab="Control")
qq1B
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/qq1B.pdf", width = 5, height = 5)
dev.off()

qq2B <- qqplot(tdat$repdem, cdat$repdem, xlab = "Treatment", ylab = "Control")
qq2B
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/qq2B.pdf", width = 5, height = 5)
dev.off()

qq3B <- qqplot(tdat$libcon, cdat$libcon, xlab = "Treatment", ylab = "Control")
qq3B
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/qq3B.pdf", width = 5, height = 5)
dev.off()

qq4B <- qqplot(tdat$demsupp, cdat$demsupp, xlab = "Treatment", ylab= "Control")
qq4B
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/qq4B.pdf", width = 5, height = 5)
dev.off()

# Gender plots
# with title
counts1 <- table(tdat$male)
counts2 <- table(cdat$male)
b1 <- barplot(counts1, main="Male (Treatent Group)", ylab="Subjects (N)")
b1
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/b1.pdf", width = 5, height = 5)
dev.off()

b2 <- barplot(counts2, main="Male (Control Group)", ylab="Subjects (N)")
b2
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/b2.pdf", width = 5, height = 5)
dev.off()

# Gender plots
# with no title
counts1B <- table(tdat$male)
counts2B <- table(cdat$male)
b1B <- barplot(counts1B, ylab="Subjects (N)")
b1B
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/b1B.pdf", width = 5, height = 5)
dev.off()

b2B <- barplot(counts2B, ylab="Subjects (N)")
b2B
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/b2B.pdf", width = 5, height = 5)
dev.off()


# Difference in means 
dif.means <- ictreg(ycount ~ 1, data = dat, treat = "treat", J=3, method = "ml")
sum.dif.means <- summary(dif.means, n.draws = 100000) # quasi-Bayesian approximation based predictions
summary(sum.dif.means)
#plot(sum.dif.means)

######################################################
# LOGISTIC DIRECT
# Direct question (treatment)
direct.q.t <- glm(voteselling ~ age + male + ethnicity + democrat + religion + libcon + demsupp, data = tdat, family = "binomial")
summary(direct.q.t)
stargazer(direct.q.t,
          type = "text", # change to "latex"/"text" when nedded
          #covariate.labels=c(
          # "High Density"),
          dep.var.labels=c("Direct Question: Vote Selling"),
          label = "direct:1",
          title = "Estimation Results (relogit)",
          font.size = "scriptsize",
          style = "apsr")

# Boostraping Regression: Treatment
set.seed(602)
b.direct.q.t <- bootCase(direct.q.t, B=2000)
bootSD.t <- apply(b.direct.q.t, 2, sd)
bootEst.t <- colMeans(b.direct.q.t)
bootCI.t <- apply(b.direct.q.t, 2, function(x) quantile(x, c(.25,.975)))
print(cbind(bootEst.t, bootSD.t, t(bootCI.t)), digits=3)

######################################################
# LOGISTIC DIRECT
# Direct question (control)
direct.q.c <- glm(voteselling ~ age + male + ethnicity + democrat + religion + libcon + demsupp, data = cdat, family = "binomial")
summary(direct.q.c)
stargazer(direct.q.c,
          type = "text", # change to "latex"/"text" when nedded
          #covariate.labels=c(
          # "High Density"),
          dep.var.labels=c("Direct Question: Vote Selling"),
          label = "direct:1",
          title = "Estimation Results (relogit)",
          font.size = "scriptsize",
          style = "apsr")

# Boostraping Regression: Control
set.seed(602)
b.direct.q.c <- bootCase(direct.q.c, B=2000)
bootSD.c <- apply(b.direct.q.c, 2, sd)
bootEst.c <- colMeans(b.direct.q.c)
bootCI.c <- apply(b.direct.q.c, 2, function(x) quantile(x, c(.25,.975)))
print(cbind(bootEst.c, bootSD.c, t(bootCI.c)), digits=3)

######################################################

# Multivariate Analysis of List Experiment
results <-ictreg(ycount ~ age + male + ethnicity + democrat + libcon + demsupp, data = dat, treat = "treat", J=3, method = "ml", maxIter = 200000)
summary(results, n.draws = 200000) # quasi-Bayesian approximation based predictions





# Average predictions
# Proportion of affirmative responses to the sensitive item ("proportions of liars estimates")
results.predicted <- predict.ictreg(results, se.fit = TRUE, interval= "confidence", avg = T, return.draws = T)
results.predicted
plot.results <- plot(results.predicted, main = "36% Sell their vote") # produces plots with estimated population proportions of respondents answering the sensitive item
plot.results
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/plot_results.pdf", width = 5, height = 5)
dev.off()

# Individual predictions
# Proportion of affirmative responses to the sensitive item ("proportions of liars estimates")
results.predicted.2 <- predict.ictreg(results, se.fit = TRUE, interval= "confidence", avg = F, return.draws = T)
results.predicted.2
plot.results.2 <- plot(results.predicted.2, main = "Individual posterior likelihoods of vote-selling") # produces plots with estimated population proportions of respondents answering the sensitive item
plot.results.2
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/plot_results2.pdf", width = 10, height = 5)
dev.off()

# Individual predictions NO TITLE
# Proportion of affirmative responses to the sensitive item ("proportions of liars estimates")
results.predicted.2B <- predict.ictreg(results, se.fit = TRUE, interval= "confidence", avg = F, return.draws = T)
results.predicted.2B
plot.results.2B <- plot(results.predicted.2B, main="Average = 36%, s.e. = (0.1068), 95% c.i.: (0.204, 0.6262)") # produces plots with estimated population proportions of respondents answering the sensitive item
plot.results.2B
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/plot_results2B.pdf", width = 10, height = 5)
dev.off()

######################################################
# Proportion of affirmative responses to the sensitive item ("proportions of liars estimates") BY DEM SUPP
demsupp1 <- demsupp2 <- demsupp3 <- demsupp4 <- demsupp5 <- demsupp6 <- demsupp7 <- dat

demsupp1[, "demsupp"] <- 1
demsupp2[, "demsupp"] <- 2
demsupp3[, "demsupp"] <- 3
demsupp4[, "demsupp"] <- 4
demsupp5[, "demsupp"] <- 5
demsupp6[, "demsupp"] <- 6
demsupp7[, "demsupp"] <- 7

avg.pred.demsupp1 <- predict.ictreg(results, newdata = demsupp1, avg = TRUE, interval = "confidence")
avg.pred.demsupp2 <- predict.ictreg(results, newdata = demsupp2, avg = TRUE, interval = "confidence")
avg.pred.demsupp3 <- predict.ictreg(results, newdata = demsupp3, avg = TRUE, interval = "confidence")
avg.pred.demsupp4 <- predict.ictreg(results, newdata = demsupp4, avg = TRUE, interval = "confidence")
avg.pred.demsupp5 <- predict.ictreg(results, newdata = demsupp5, avg = TRUE, interval = "confidence")
avg.pred.demsupp6 <- predict.ictreg(results, newdata = demsupp6, avg = TRUE, interval = "confidence")
avg.pred.demsupp0 <- predict.ictreg(results, newdata = demsupp7, avg = TRUE, interval = "confidence")


demsup.prop <- plot(c(avg.pred.demsupp0, avg.pred.demsupp1, avg.pred.demsupp2, avg.pred.demsupp3, avg.pred.demsupp4, avg.pred.demsupp5, avg.pred.demsupp6), labels = c(
  "Disagree", "2", "3", "4", "5", "6", "Agree"), xlab="Democracy is not better than other forms of government")

demsup.prop
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/demsup_prop.pdf", width = 10, height = 5)
dev.off()

######################################################
libcon1 <- libcon2 <- libcon3 <- libcon4 <- libcon5 <- libcon6  <- libcon7 <-  dat

libcon1[, "libcon"] <- 1
libcon2[, "libcon"] <- 2
libcon3[, "libcon"] <- 3
libcon4[, "libcon"] <- 4
libcon5[, "libcon"] <- 5
libcon6[, "libcon"] <- 6
libcon7[, "libcon"] <- 7

avg.pred.libcon1 <- predict.ictreg(results, newdata = libcon1, avg = TRUE, interval = "confidence")
avg.pred.libcon2 <- predict.ictreg(results, newdata = libcon2, avg = TRUE, interval = "confidence")
avg.pred.libcon3 <- predict.ictreg(results, newdata = libcon3, avg = TRUE, interval = "confidence")
avg.pred.libcon4 <- predict.ictreg(results, newdata = libcon4, avg = TRUE, interval = "confidence")
avg.pred.libcon5 <- predict.ictreg(results, newdata = libcon5, avg = TRUE, interval = "confidence")
avg.pred.libcon6 <- predict.ictreg(results, newdata = libcon6, avg = TRUE, interval = "confidence")
avg.pred.libcon7 <- predict.ictreg(results, newdata = libcon7, avg = TRUE, interval = "confidence")

libcon.prop <- plot(c(avg.pred.libcon1, avg.pred.libcon2, avg.pred.libcon3, avg.pred.libcon4, avg.pred.libcon5, avg.pred.libcon6, avg.pred.libcon7), labels = c(
  "Lib", "2", "3", "4", "5", "6", "Con"), xlab="Liberal/Conservative")

libcon.prop
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/libcon_prop.pdf", width = 10, height = 5)
dev.off()

######################################################
# Proportions of liars estimates" by party 

repdem1 <- repdem2 <- repdem3 <- repdem4 <- repdem5 <- repdem6 <- repdem7  <- dat

repdem1[, "repdem"] <- 1
repdem2[, "repdem"] <- 2
repdem3[, "repdem"] <- 3
repdem4[, "repdem"] <- 4
repdem5[, "repdem"] <- 5
repdem6[, "repdem"] <- 6
repdem7[, "repdem"] <- 7

avg.pred.repdem1 <- predict.ictreg(results, newdata = repdem1, avg = TRUE, interval = "confidence")
avg.pred.repdem2 <- predict.ictreg(results, newdata = repdem2, avg = TRUE, interval = "confidence")
avg.pred.repdem3 <- predict.ictreg(results, newdata = repdem3, avg = TRUE, interval = "confidence")
avg.pred.repdem4 <- predict.ictreg(results, newdata = repdem4, avg = TRUE, interval = "confidence")
avg.pred.repdem5 <- predict.ictreg(results, newdata = repdem5, avg = TRUE, interval = "confidence")
avg.pred.repdem6 <- predict.ictreg(results, newdata = repdem6, avg = TRUE, interval = "confidence")
avg.pred.repdem7 <- predict.ictreg(results, newdata = repdem7, avg = TRUE, interval = "confidence")

repdem.prop <- plot(c(avg.pred.repdem1, avg.pred.repdem2, avg.pred.repdem3, avg.pred.repdem4, avg.pred.repdem5, avg.pred.repdem6, avg.pred.repdem7), 
                    labels = c("SD", "WD", "ID", "II", "IR", "WR", "SR"), xlab="Democratic-Republican Scale", asp=4:2)

repdem.prop
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/repdem_prop.pdf", width = 8, height = 5)
dev.off()

######################################################
age17 <- age18 <- age19 <- age20 <- age21 <- age22 <- age23  <- age24 <- age25 <- age26 <- age27 <- age42 <- dat

age17[, "age"] <- 17
age18[, "age"] <- 18
age19[, "age"] <- 19
age20[, "age"] <- 20
age21[, "age"] <- 21
age22[, "age"] <- 22
age23[, "age"] <- 23
age24[, "age"] <- 24
age25[, "age"] <- 25
age26[, "age"] <- 26
age27[, "age"] <- 27
age42[, "age"] <- 42

avg.pred.age17 <- predict.ictreg(results, newdata = age17, avg = TRUE, interval = "confidence")

avg.pred.age18 <- predict.ictreg(results, newdata = age18, avg = TRUE, interval = "confidence")

avg.pred.age19 <- predict.ictreg(results, newdata = age19, avg = TRUE, interval = "confidence")

avg.pred.age20 <- predict.ictreg(results, newdata = age20, avg = TRUE, interval = "confidence")

avg.pred.age21 <- predict.ictreg(results, newdata = age21, avg = TRUE, interval = "confidence")

avg.pred.age22 <- predict.ictreg(results, newdata = age22, avg = TRUE, interval = "confidence")

avg.pred.age23 <- predict.ictreg(results, newdata = age23, avg = TRUE, interval = "confidence")

avg.pred.age24 <- predict.ictreg(results, newdata = age24, avg = TRUE, interval = "confidence")

avg.pred.age25 <- predict.ictreg(results, newdata = age25, avg = TRUE, interval = "confidence")

avg.pred.age26 <- predict.ictreg(results, newdata = age26, avg = TRUE, interval = "confidence")

avg.pred.age27 <- predict.ictreg(results, newdata = age27, avg = TRUE, interval = "confidence")

avg.pred.age42 <- predict.ictreg(results, newdata = age42, avg = TRUE, interval = "confidence")


age.prop <- plot(c(avg.pred.age17, avg.pred.age18, avg.pred.age19, avg.pred.age20, avg.pred.age21, avg.pred.age22, avg.pred.age23, avg.pred.age24, avg.pred.age25, avg.pred.age26, avg.pred.age27, avg.pred.age42), 
                 labels = c("17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "42"), xlab="Age", asp=4:2)

age.prop
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/age_prop.pdf", width = 8, height = 5)
dev.off()

######################################################
# Design effects

ycount <- dat$ycount # Extracting vectors
treat <- dat$treat # Extracting vectors
treat <- factor(treat, labels = c("F", "T")) # Recoding treat as factor
treat <- as.logical(treat) # Recoding treat as logical vector

design <- ict.test(ycount, treat, J=3, gms = TRUE, n.draws = 250000, alpha = 0.05, pi.table = TRUE)
print(design) # no design effects

###############################################
# Vote Selling direct question list experiment
###############################################
# with title
c.plot <- table(cdat$voteselling1)
c.plot <- barplot(c.plot, main="Vote-Selling (control subjects)", ylab="Subjects (N)")
c.plot
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/ashamed.pdf", width = 5, height = 5)
dev.off()

# WITHOUT title
c.plotB <- table(cdat$voteselling1)
c.plotB <- barplot(c.plotB, ylab="Subjects (N)")
c.plotB
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/ashamedB.pdf", width = 5, height = 5)
dev.off()


###############################################
# Direct CLIENTELISM question plot from LAPOP
###############################################
load("/Users/hectorbahamonde/RU/Term5/Experiments_Redlawsk/Experiment/Data/LAPOP/datLAPOP.rdata")
datLAPOP$clien1 <- factor(datLAPOP$clien1, labels = c("Often", "Sometimes", "Never"))

clien1 <- table(datLAPOP$clien1)
clien1 <- barplot(clien1, ylab="Subjects (N)")
clien1
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/clien1.pdf", width = 5, height = 5)
dev.off()

###############################################
# Vote-selling Pricing Survey Plot
###############################################
pricing2 <- table(dat$buying2)
pricing2 <- barplot(pricing2, xlab="Price", ylab="Subjects (N)")
pricing2
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/pricing2.pdf", width = 5, height = 5)
dev.off()

set.seed(602)
pricing1 <- xyplot(jitter(dat$buying2) ~ jitter(dat$demsupp) | dat$treat, type = c("p", "smooth"), lwd = 2, xlab="Level of disagreement in that democracy has problems, but it's better than other forms of Gov't.", ylab="Right price for your vote")
pricing1
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/pricing1.pdf", width = 10, height = 5)
dev.off()

set.seed(602)
xyplot(jitter(dat$buying1) ~ jitter(dat$demsupp) | dat$treat, type = c("p", "smooth"), lwd = 2, xlab="Level of disagreement in that democracy has problems, but it's better than other forms of Gov't.", ylab="Too little money for your vote")


###############################################
# Income plot
###############################################
income <- table(dat$income)
income <- barplot(income, xlab="Anual income", ylab="Subjects (N)")
income
dev.copy(pdf, "/Users/hectorbahamonde/RU/research/Vote_Selling/Paper/income.pdf", width = 8, height = 8)
dev.off()