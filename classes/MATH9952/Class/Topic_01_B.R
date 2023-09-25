# setwd("~/joe/lectures/DATA")



### doseresponse 
dr = read.table("~/Workspace/College/DIT/MATH9952/Data/txt/doseresponse.txt", header = T, sep = ' ')
attach(dr)

fit_dr2 = lm(activity ~ dose + I(dose^2), data = dr)

# source('~/joe/lectures/Regression\ Models\ I/Lectures/anovatab.R')
# anovatab(fit_dr2)

anova(fit_dr2)

fit_dr3 = update(fit_dr2, . ~ . + I(dose^3))
drop1(fit_dr3, test = 'F')

fit_dr4 = update(fit_dr3, . ~ . + I(dose^4))
drop1(fit_dr4, test = 'F')

fit_dr5 = update(fit_dr4, . ~ . + I(dose^5))
drop1(fit_dr5, test = 'F')







#### fitness data
fitness=read.table("fitness.txt",header=T)
colnames(fitness)=tolower(colnames(fitness))

#install.packages('leaps')
library(leaps)

X=as.matrix(fitness[,-3])
allregs=leaps(x=X, y=fitness$oxygen, int=TRUE, method=c("r2"),nbest=20)
allregs

colnames(allregs$which)=colnames(X)

allregs=cbind(allregs$which,allregs$size,allregs$r2)
allregs[order(-allregs[,8]),]

#### forward using add1 slentry=0.25 ####
fit=lm(oxygen~1,data=fitness)
scope=~age+weight+runtime+restpulse+runpulse+maxpulse

add1(fit,scope,test='F')
add1(update(fit,.~+runtime),scope,test='F')
add1(update(fit,.~+runtime+age),scope,test='F')
add1(update(fit,.~+runtime+age+runpulse),scope,test='F')
add1(update(fit,.~+runtime+age+runpulse+maxpulse),scope,test='F')
add1(update(fit,.~+runtime+age+runpulse+maxpulse+weight),scope,test='F')

ffinal=update(fit,.~+runtime+age+runpulse+maxpulse+weight)

summary(ffinal)

#### backward using drop1 slstay=0.05 ####
fit1b=lm(oxygen~age+weight+runtime+restpulse+runpulse+maxpulse,data=fitness)

drop1(fit1b,test='F')
drop1(update(fit1b,~.-restpulse),test='F')
drop1(update(fit1b,~.-restpulse-weight),test='F')
drop1(update(fit1b,~.-restpulse-weight-maxpulse),test='F')

bfinal=update(fit1b,~.-restpulse-weight-maxpulse)

summary(bfinal)

#### stepwise using slentry=0.25 slstay=0.15 ####
fit=lm(oxygen~1,data=fitness)
scope=~age+weight+runtime+restpulse+runpulse+maxpulse

add1(fit,scope,test='F')
add1(update(fit,.~+runtime),scope,test='F')
drop1(update(fit,.~+runtime+age),test='F')
add1(update(fit,.~+runtime+age),scope,test='F')
drop1(update(fit,.~+runtime+age+runpulse),test='F')
add1(update(fit,.~+runtime+age+runpulse),scope,test='F')
drop1(update(fit,.~+runtime+age+runpulse+maxpulse),test='F')
add1(update(fit,.~+runtime+age+runpulse+maxpulse),scope,test='F')
drop1(update(fit,.~+runtime+age+runpulse+maxpulse+weight),test='F')

sfinal=update(fit,.~+runtime+age+runpulse+maxpulse)

summary(sfinal)




##########################################################
# Alternate methods using AIC - function step()
##########################################################
fit=lm(oxygen~1,data=fitness)
fit1b=lm(oxygen~age+weight+runtime+restpulse+runpulse+maxpulse,data=fitness)

step(fit,scope=list(lower=~1,upper=~age+weight+runtime+restpulse+runpulse+maxpulse),direction="forward",trace=1)
step(fit1b,scope=list(lower=~1,upper=~age+weight+runtime+restpulse+runpulse+maxpulse),direction="backward",trace=1)
step(fit,scope=list(lower=~1,upper=~age+weight+runtime+restpulse+runpulse+maxpulse),direction="both",trace=1)




##########################################################
### Mallow's Cp
##########################################################
#install.packages('leaps')
library(leaps)
X=as.matrix(fitness[,-3])

allregs=leaps(x=X, y=fitness$oxygen, int=TRUE, method=c("Cp"),nbest=20)
allregs

colnames(allregs$which)=colnames(X)

allregs=cbind(allregs$which,allregs$size,allregs$Cp)
allregs[order(-allregs[,8]),]
