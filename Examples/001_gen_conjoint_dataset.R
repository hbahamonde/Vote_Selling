#####################################################################
# Generate conjoint dataset
#####################################################################

rm(list=ls())

library(foreign)
library(Hmisc)
library(xlsx)
library(gdata)

# Summary of datasets

# c = merged conjoint
# e = survey data
# d = conjoint data

# Load splited conjoint 
c = read.dta("~/Dropbox/Atacama 2015/04_fieldwork/survey materials/merged_conjoint.dta")
c$code = paste(c$idnum,c$pair,c$candidate,sep="")
c$code = as.numeric(c$code)
c$code = as.numeric(c$code)
names(c)
head(c)
dim(c)

# Explore data
describe(c$descripcion)
describe(c$caracteristica)
describe(c$idnum)
describe(c$pair)
describe(c$candidate)
head(c)

# Check probabilities
y30 = sum(c$caracteristica=="30")
y40 = sum(c$caracteristica=="40")
y50 = sum(c$caracteristica=="50")
p30 = y30/(y30+y40+y50)
p40 = y40/(y30+y40+y50)
p50 = y50/(y30+y40+y50)
p30
p40
p50

hombre = sum(c$caracteristica=="Hombre")
mujer = sum(c$caracteristica=="Mujer")
phombre = hombre/(hombre+mujer)
pmujer =  mujer/(hombre+mujer)
phombre
pmujer

ayuda = sum(c$caracteristica=="Entregar ayuda economica")
noayuda = sum(c$caracteristica=="NO entregar ayuda economica")
payuda = ayuda/(ayuda+noayuda)
pnoayuda = noayuda/(ayuda+noayuda)
payuda
pnoayuda

sinexp = sum(c$caracteristica=="Sin experiencia")
concejal = sum(c$caracteristica=="Concejal")
alcalde = sum(c$caracteristica=="Alcalde")
psinexp = sinexp/(sinexp+concejal+alcalde)
pconcejal =  concejal/(sinexp+concejal+alcalde)
palcalde =  alcalde/(sinexp+concejal+alcalde)
psinexp 
pconcejal
palcalde

centro = sum(c$caracteristica=="Centro")
derecha = sum(c$caracteristica=="Derecha")
izquierda = sum(c$caracteristica=="Izquierda")
independiente = sum(c$caracteristica=="Independiente")
pcentro = centro/(centro+derecha+izquierda+independiente)
pderecha = derecha/(centro+derecha+izquierda+independiente)
pizquierda = izquierda/(centro+derecha+izquierda+independiente)
pindependiente =  independiente/(centro+derecha+izquierda+independiente)
pcentro
pderecha
pizquierda
pindependiente

ingeniero = sum(c$caracteristica=="Ingeniero(a)")
jardinero = sum(c$caracteristica=="Jardinero(a)")
profesor = sum(c$caracteristica=="Profesor(a)")
pingeniero = ingeniero/(ingeniero+jardinero+profesor)
pjardinero = jardinero/(ingeniero+jardinero+profesor)
pprofesor = profesor/(ingeniero+jardinero+profesor)
pingeniero
pjardinero
pprofesor

# Gen conjoint dataset
idnum = rep(1:210, each = 16)
pair = rep(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8), times = 210)
candidate = rep(c(1,2), times = 1680)
atideology = rep(NA,3360)
atprofession = rep(NA,3360)
atgender = rep(NA,3360)
atage = rep(NA,3360)
atexperience = rep(NA,3360)
atexpectations = rep(NA,3360)
selected = rep(NA,3360)
code = paste(idnum,pair,candidate,sep = "")
code = as.numeric(code)
d = data.frame(code,idnum,pair,candidate,atideology,atprofession,atgender,atage,atexperience,atexpectations,selected)
names(d)

head(d)
head(c)

# Manually generating conjoint dataset to the loop
d$atideology[d$code==111] = c$caracteristica[c$descripcion=="ideologia" & c$code==111]
d$atideology[d$code==112] = c$caracteristica[c$descripcion=="ideologia" & c$code==112]
d$atideology[d$code==121] = c$caracteristica[c$descripcion=="ideologia" & c$code==121]
d$atideology[d$code==122] = c$caracteristica[c$descripcion=="ideologia" & c$code==122]
d$atideology[d$code==131] = c$caracteristica[c$descripcion=="ideologia" & c$code==131]
d$atideology[d$code==132] = c$caracteristica[c$descripcion=="ideologia" & c$code==132]
head(d)

# Loop for ideology

for (i in code) {
  
d$atideology[d$code==i] = c$caracteristica[c$descripcion=="ideologia" & c$code==i]
  
}

# Loop for profession

for (i in code) {
  
  d$atprofession[d$code==i] = c$caracteristica[c$descripcion=="profesion" & c$code==i]
  
}

# Loop for gender

for (i in code) {
  
  d$atgender[d$code==i] = c$caracteristica[c$descripcion=="genero" & c$code==i]
  
}

# Loop for age

for (i in code) {
  
  d$atage[d$code==i] = c$caracteristica[c$descripcion=="edad" & c$code==i]
  
}

# Loop for experience

for (i in code) {
  
  d$atexperience[d$code==i] = c$caracteristica[c$descripcion=="experiencia" & c$code==i]
  
}

# Loop for expectations

for (i in code) {
    
  d$atexpectations[d$code==i] = c$caracteristica[c$descripcion=="propuesta" & c$code==i]
  
}

# Check data
head(d)
describe(d$atideology)
describe(d$atprofession) 
describe(d$atgender) 
describe(d$atage)    
describe(d$atexperience)              
describe(d$atexpectations)

# Load survey data
e = read.dta("~/Dropbox/Atacama 2015/01_data/clean data/survey_paipote.dta")
head(e)

# Gen outcome dataset
idnum = rep(1:210, times = 16)
pair = rep(1:8, each=420)
candidate = rep(c(1:2), each=210, times = 8)
zone.0 = as.factor(e$zone)
zone = c(zone.0,zone.0,zone.0,zone.0,zone.0,zone.0,zone.0,zone.0,zone.0,zone.0,zone.0,zone.0,zone.0,zone.0,zone.0,zone.0)

a1.0 = c(e$a1)
a1 = c(a1.0,a1.0,a1.0,a1.0,a1.0,a1.0,a1.0,a1.0,a1.0,a1.0,a1.0,a1.0,a1.0,a1.0,a1.0,a1.0)
a2.0 = c(e$a2) 
a2 = c(a2.0,a2.0,a2.0,a2.0,a2.0,a2.0,a2.0,a2.0,a2.0,a2.0,a2.0,a2.0,a2.0,a2.0,a2.0,a2.0)
a3.0 = c(e$a3) 
a3 = c(a3.0,a3.0,a3.0,a3.0,a3.0,a3.0,a3.0,a3.0,a3.0,a3.0,a3.0,a3.0,a3.0,a3.0,a3.0,a3.0)
a4.0 = c(e$a4) 
a4 = c(a4.0,a4.0,a4.0,a4.0,a4.0,a4.0,a4.0,a4.0,a4.0,a4.0,a4.0,a4.0,a4.0,a4.0,a4.0,a4.0)
a5.0 = c(e$a5) 
a5 = c(a5.0,a5.0,a5.0,a5.0,a5.0,a5.0,a5.0,a5.0,a5.0,a5.0,a5.0,a5.0,a5.0,a5.0,a5.0,a5.0)
a6.0 = c(e$a6) 
a6 = c(a6.0,a6.0,a6.0,a6.0,a6.0,a6.0,a6.0,a6.0,a6.0,a6.0,a6.0,a6.0,a6.0,a6.0,a6.0,a6.0)

b1.0 = c(e$b1)  
b1 = c(b1.0,b1.0,b1.0,b1.0,b1.0,b1.0,b1.0,b1.0,b1.0,b1.0,b1.0,b1.0,b1.0,b1.0,b1.0,b1.0)   
b2.0 = c(e$b2)  
b2 = c(b2.0,b2.0,b2.0,b2.0,b2.0,b2.0,b2.0,b2.0,b2.0,b2.0,b2.0,b2.0,b2.0,b2.0,b2.0,b2.0)   
b3.0 = c(e$b3)  
b3 = c(b3.0,b3.0,b3.0,b3.0,b3.0,b3.0,b3.0,b3.0,b3.0,b3.0,b3.0,b3.0,b3.0,b3.0,b3.0,b3.0)   
b4.0 = c(e$b4)  
b4 = c(b4.0,b4.0,b4.0,b4.0,b4.0,b4.0,b4.0,b4.0,b4.0,b4.0,b4.0,b4.0,b4.0,b4.0,b4.0,b4.0)   
b5.0 = c(e$b5)  
b5 = c(b5.0,b5.0,b5.0,b5.0,b5.0,b5.0,b5.0,b5.0,b5.0,b5.0,b5.0,b5.0,b5.0,b5.0,b5.0,b5.0)   
b6.0 = c(e$b6)  
b6 = c(b6.0,b6.0,b6.0,b6.0,b6.0,b6.0,b6.0,b6.0,b6.0,b6.0,b6.0,b6.0,b6.0,b6.0,b6.0,b6.0)   
b7.0 = c(e$b7)  
b7 = c(b7.0,b7.0,b7.0,b7.0,b7.0,b7.0,b7.0,b7.0,b7.0,b7.0,b7.0,b7.0,b7.0,b7.0,b7.0,b7.0)   
b8.0 = c(e$b8)  
b8 = c(b8.0,b8.0,b8.0,b8.0,b8.0,b8.0,b8.0,b8.0,b8.0,b8.0,b8.0,b8.0,b8.0,b8.0,b8.0,b8.0)   
b9.0 = c(e$b9)  
b9 = c(b9.0,b9.0,b9.0,b9.0,b9.0,b9.0,b9.0,b9.0,b9.0,b9.0,b9.0,b9.0,b9.0,b9.0,b9.0,b9.0)   
b10.0 = c(e$b10)  
b10 = c(b10.0,b10.0,b10.0,b10.0,b10.0,b10.0,b10.0,b10.0,b10.0,b10.0,b10.0,b10.0,b10.0,b10.0,b10.0,b10.0)   
b11.0 = c(e$b11) 
b11 = c(b11.0,b11.0,b11.0,b11.0,b11.0,b11.0,b11.0,b11.0,b11.0,b11.0,b11.0,b11.0,b11.0,b11.0,b11.0,b11.0)
b12.0 = c(e$b12) 
b12 = c(b12.0,b12.0,b12.0,b12.0,b12.0,b12.0,b12.0,b12.0,b12.0,b12.0,b12.0,b12.0,b12.0,b12.0,b12.0,b12.0)
b13.0 = c(e$b13) 
b13 = c(b13.0,b13.0,b13.0,b13.0,b13.0,b13.0,b13.0,b13.0,b13.0,b13.0,b13.0,b13.0,b13.0,b13.0,b13.0,b13.0)
b14.0 = c(e$b14) 
b14 = c(b14.0,b14.0,b14.0,b14.0,b14.0,b14.0,b14.0,b14.0,b14.0,b14.0,b14.0,b14.0,b14.0,b14.0,b14.0,b14.0)

c1.0 = c(e$c1) 
c1 = c(c1.0,c1.0,c1.0,c1.0,c1.0,c1.0,c1.0,c1.0,c1.0,c1.0,c1.0,c1.0,c1.0,c1.0,c1.0,c1.0)
c2.0 = c(e$c2) 
c2 = c(c2.0,c2.0,c2.0,c2.0,c2.0,c2.0,c2.0,c2.0,c2.0,c2.0,c2.0,c2.0,c2.0,c2.0,c2.0,c2.0)
c3.0 = c(e$c3) 
c3 = c(c3.0,c3.0,c3.0,c3.0,c3.0,c3.0,c3.0,c3.0,c3.0,c3.0,c3.0,c3.0,c3.0,c3.0,c3.0,c3.0)
c4.0 = c(e$c4) 
c4 = c(c4.0,c4.0,c4.0,c4.0,c4.0,c4.0,c4.0,c4.0,c4.0,c4.0,c4.0,c4.0,c4.0,c4.0,c4.0,c4.0)
c5.0 = c(e$c5) 
c5 = c(c5.0,c5.0,c5.0,c5.0,c5.0,c5.0,c5.0,c5.0,c5.0,c5.0,c5.0,c5.0,c5.0,c5.0,c5.0,c5.0)
c6.0 = c(e$c6) 
c6 = c(c6.0,c6.0,c6.0,c6.0,c6.0,c6.0,c6.0,c6.0,c6.0,c6.0,c6.0,c6.0,c6.0,c6.0,c6.0,c6.0)

d1.0 = c(e$d1)
d1 = c(d1.0,d1.0)
d2.0 = c(e$d2)
d2 = c(d2.0,d2.0)
d3.0 = c(e$d3)
d3 = c(d3.0,d3.0)
d4.0 = c(e$d4)
d4 = c(d4.0,d4.0)
d5.0 = c(e$d5)
d5 = c(d5.0,d5.0)
d6.0 = c(e$d6)
d6 = c(d6.0,d6.0)
d7.0 = c(e$d7)
d7 = c(d7.0,d7.0)
d8.0 = c(e$d8)
d8 = c(d8.0,d8.0)
failcon.0 = c(e$failcon)
failcon = c(failcon.0,failcon.0,failcon.0,failcon.0,failcon.0,failcon.0,failcon.0,failcon.0,failcon.0,failcon.0,failcon.0,failcon.0,failcon.0,failcon.0,failcon.0,failcon.0)

all_d = c(d1,d2,d3,d4,d5,d6,d7,d8)
code = paste(idnum,pair,candidate,sep = "")
code = as.numeric(code)
outcome = data.frame(code,idnum,pair,candidate,all_d,zone,a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6,b7,
                     b8,b9,b10,b11,b12,b13,b14,c1,c2,c3,c4,c5,c6,d1,d2,d3,d4,d5,d6,d7,d8,failcon)
head(outcome)

d$a1 = rep(NA,3360) 
d$a2 = rep(NA,3360) 
d$a3 = rep(NA,3360) 
d$a4 = rep(NA,3360) 
d$a5 = rep(NA,3360) 
d$a6 = rep(NA,3360) 
d$b1 = rep(NA,3360) 
d$b2 = rep(NA,3360) 
d$b3 = rep(NA,3360) 
d$b4 = rep(NA,3360) 
d$b5 = rep(NA,3360) 
d$b6 = rep(NA,3360) 
d$b7 = rep(NA,3360) 
d$b8 = rep(NA,3360) 
d$b9 = rep(NA,3360) 
d$b10 = rep(NA,3360) 
d$b11 = rep(NA,3360) 
d$b12 = rep(NA,3360) 
d$b13 = rep(NA,3360) 
d$b14 = rep(NA,3360) 
d$c1 = rep(NA,3360) 
d$c2 = rep(NA,3360) 
d$c3 = rep(NA,3360) 
d$c4 = rep(NA,3360) 
d$c5 = rep(NA,3360) 
d$c6 = rep(NA,3360) 
d$selected2 = rep(NA,3360)
d$zone = rep(NA,3360)
d$failcon = rep(NA,3360)

# selected2
for (i in code) {
 d$selected2[d$code==i] = outcome$all_d[outcome$code==i] 
}

# zone
for (i in code) {
  d$zone[d$code==i] = outcome$zone[outcome$code==i] 
}

# a1
for (i in code) {
  d$a1[d$code==i] = outcome$a1[outcome$code==i] 
}

# a2
for (i in code) {
  d$a2[d$code==i] = outcome$a2[outcome$code==i] 
}

# a3
for (i in code) {
  d$a3[d$code==i] = outcome$a3[outcome$code==i] 
}

# a4
for (i in code) {
  d$a4[d$code==i] = outcome$a4[outcome$code==i] 
}

# a5
for (i in code) {
  d$a5[d$code==i] = outcome$a5[outcome$code==i] 
}

# a6
for (i in code) {
  d$a6[d$code==i] = outcome$a6[outcome$code==i] 
}

# b1
for (i in code) {
  d$b1[d$code==i] = outcome$b1[outcome$code==i] 
}

# b2
for (i in code) {
  d$b2[d$code==i] = outcome$b2[outcome$code==i] 
}

# b3
for (i in code) {
  d$b3[d$code==i] = outcome$b3[outcome$code==i] 
}

# b4
for (i in code) {
  d$b4[d$code==i] = outcome$b4[outcome$code==i] 
}

# b5
for (i in code) {
  d$b5[d$code==i] = outcome$b5[outcome$code==i] 
}

# b6
for (i in code) {
  d$b6[d$code==i] = outcome$b6[outcome$code==i] 
}

# b7
for (i in code) {
  d$b7[d$code==i] = outcome$b7[outcome$code==i] 
}

# b8
for (i in code) {
  d$b8[d$code==i] = outcome$b8[outcome$code==i] 
}

# b9
for (i in code) {
  d$b9[d$code==i] = outcome$b9[outcome$code==i] 
}

# b10
for (i in code) {
  d$b10[d$code==i] = outcome$b10[outcome$code==i] 
}

# b11
for (i in code) {
  d$b11[d$code==i] = outcome$b11[outcome$code==i] 
}

# b12
for (i in code) {
  d$b12[d$code==i] = outcome$b12[outcome$code==i] 
}

# b13
for (i in code) {
  d$b13[d$code==i] = outcome$b13[outcome$code==i] 
}

# b14
for (i in code) {
  d$b14[d$code==i] = outcome$b14[outcome$code==i] 
}

# c1
for (i in code) {
  d$c1[d$code==i] = outcome$c1[outcome$code==i] 
}

# c2
for (i in code) {
  d$c2[d$code==i] = outcome$c2[outcome$code==i] 
}

# c3
for (i in code) {
  d$c3[d$code==i] = outcome$c3[outcome$code==i] 
}

# c4
for (i in code) {
  d$c4[d$code==i] = outcome$c4[outcome$code==i] 
}

# c5
for (i in code) {
  d$c5[d$code==i] = outcome$c5[outcome$code==i] 
}

# c6
for (i in code) {
  d$c6[d$code==i] = outcome$c6[outcome$code==i] 
}

# failcon
for (i in code) {
  d$failcon[d$code==i] = outcome$failcon[outcome$code==i] 
}

# selected
yes = 1
no = 0
d$vote = ifelse(d$candidate == d$selected2, yes, no)
d$vote[d$selected2==88]=999
d$vote[d$selected2==99]=999
d$selected = d$vote
d$selected2 = NULL
d$vote = NULL

# Gen restricted sample
d_1 = d[d$candidate==1, ]
d_2 = d[d$candidate==2, ]

d_1$atexpectations[d_1$atexpectations=="NO entregar ayuda economica"] = 0
d_1$atexpectations[d_1$atexpectations=="Entregar ayuda economica"] = 1
d_1$atexpectations = as.numeric(d_1$atexpectations)

d_2$atexpectations[d_2$atexpectations=="NO entregar ayuda economica"] = 0
d_2$atexpectations[d_2$atexpectations=="Entregar ayuda economica"] = 1
d_2$atexpectations = as.numeric(d_2$atexpectations)

same_exp_2 = d_1$atexpectations - d_2$atexpectations
same_exp = NA
same_exp[same_exp_2== 0] = 1
same_exp[same_exp_2==-1] = 0
same_exp[same_exp_2== 1] = 0
table(same_exp)

d_1$same_exp = same_exp
d_2$same_exp = same_exp
d3 = rbind(d_1,d_2)
d$same_exp = rep(NA,3360) 
d3 = d3[order(d3$code, decreasing=FALSE), ]
d$same_exp[d3$same_exp==1]=1
d$same_exp[d3$same_exp==0]=0
table(d$same_exp)

# Prepare labels
d$atideology
d$atideology[d$atideology=="Independiente"] = 1
d$atideology[d$atideology=="Centro"] = 2
d$atideology[d$atideology=="Derecha"] = 3
d$atideology[d$atideology=="Izquierda"] = 4
d$atideology = as.numeric(d$atideology)
d$atideology

d$atgender
d$atgender[d$atgender=="Hombre"] = 1
d$atgender[d$atgender=="Mujer"] = 2
d$atgender = as.numeric(d$atgender)
d$atgender

d$atprofession
d$atprofession[d$atprofession=="Jardinero(a)"] = 1
d$atprofession[d$atprofession=="Profesor(a)"] = 2
d$atprofession[d$atprofession=="Ingeniero(a)"] = 3
d$atprofession = as.numeric(d$atprofession)
d$atprofession

d$atage
d$atage[d$atage=="30"] = 1
d$atage[d$atage=="40"] = 2
d$atage[d$atage=="50"] = 3
d$atage = as.numeric(d$atage)
d$atage

d$atexperience
d$atexperience[d$atexperience=="Sin experiencia"] = 1
d$atexperience[d$atexperience=="Concejal"] = 2
d$atexperience[d$atexperience=="Alcalde"] = 3
d$atexperience = as.numeric(d$atexperience)
d$atexperience

d$atexpectations
d$atexpectations[d$atexpectations=="NO entregar ayuda economica"] = 1
d$atexpectations[d$atexpectations=="Entregar ayuda economica"] = 2
d$atexpectations = as.numeric(d$atexpectations)
d$atexpectations

# Check
table(d$atideology,d$selected)
table(d$atgender,d$selected)
table(d$atprofession,d$selected)
table(d$atage,d$selected)
table(d$atexperience,d$selected)
table(d$atexpectations,d$selected)

# idnum
d$idnum = as.character(d$idnum)

# Save matched data set
write.dta(d, file = "~/Dropbox/Atacama 2015/01_data/clean data/conjoint_paipote.dta")

# Labels in Stata
