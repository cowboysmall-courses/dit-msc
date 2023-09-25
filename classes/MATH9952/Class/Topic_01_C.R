# setwd('~/joe/lectures/DATA')
######## Clinical data ######################################################
#### Clinical data
response=c(91,97,104,112,115,114,119,116,115)
treat=c(rep(c(1,2,3),rep(3,3)))
fit4=lm(response~factor(treat))
summary(fit4)
drop1(fit4,test='F')

# install.packages('multcomp')
library(multcomp)
L1=diag(3)[-1,]
glh1=glht(fit4,linfct=L1)
summary(glh1,test=Ftest())

L2=matrix(c(0,1,-1),nrow=1)
glh2=glht(fit4,linfct=L2)
summary(glh2,test=Ftest())
confint(glh2)


# the L1 test can be done manually via:
L=L1
glhm=t(L%*%fit4$coeff)%*%solve(L%*%vcov(fit4)%*%t(L))%*%(L%*%fit4$coef)/dim(L)[1]
p_value=1-pf(glhm,2,6);p_value;


######## Turkey Data    #####################################################

turkey = read.table("turkey.txt",header=T,sep='')
fit5=lm(weight~factor(feed)+age,data=turkey)
summary(fit5)
drop1(fit5,test='F')
model.matrix(fit5)
### interaction model
fit5a=lm(weight~factor(feed)*age,data=turkey)
model.matrix(fit5a)
summary(fit5a)
drop1(fit5a,test='F')
