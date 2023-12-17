"VARIABLE DESCRIPTIONS:
survival        Survival
                (0 = No; 1 = Yes)
pclass          Passenger Class
(1 = 1st; 2 = 2nd; 3 = 3rd)
name            Name
sex             Sex
age             Age
sibsp           Number of Siblings/Spouses Aboard
parch           Number of Parents/Children Aboard
ticket          Ticket Number
fare            Passenger Fare
cabin           Cabin
embarked        Port of Embarkation
(C = Cherbourg; Q = Queenstown; S = Southampton)

SPECIAL NOTES:
Pclass is a proxy for socio-economic status (SES)
1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower

Age is in Years; Fractional if Age less than One (1)
If the Age is Estimated, it is in the form xx.5

With respect to the family relation variables (i.e. sibsp and parch)
some relations were ignored.  The following are the definitions used
for sibsp and parch.

Sibling:  Brother, Sister, Stepbrother, or Stepsister of Passenger Aboard Titanic
Spouse:   Husband or Wife of Passenger Aboard Titanic (Mistresses and Fiances Ignored)
Parent:   Mother or Father of Passenger Aboard Titanic
Child:    Son, Daughter, Stepson, or Stepdaughter of Passenger Aboard Titanic

Other family relatives excluded from this study include cousins,
nephews/nieces, aunts/uncles, and in-laws.  Some children travelled
only with a nanny, therefore parch=0 for them.  As well, some
travelled with very close friends or neighbors in a village, however,
the definitions do not support such relations."

#########################################################################
setwd("~/joe/lectures/DATA")
data=read.csv("titanic.csv",header=T)

attributes(data)$names=tolower(attributes(data)$names)

names=attributes(data)$names
names

### reproducible random stream 
set.seed(10123456)
include=rbinom(dim(data)[1],1,2/3)
train=subset(data,include==1)
validation=subset(data,include==0)


## data split 
attach(validation)


## most naive model - just flip a coin!
guess=rbinom(nrow(validation),1,.5)
xtabs(~survived+guess)
prop.table(xtabs(~survived+guess))


### slightly better guess
p=sum(train$survived)/nrow(train)
guess2=rbinom(nrow(validation),1,p)
xtabs(~survived+guess2)
prop.table(xtabs(~survived+guess2))
detach(validation)


## try logistic regression 
## small model
fit=glm(survived~age+sex,family=binomial(),data=train);
summary(fit)
predicted=predict(fit,newdata=validation,type="response")
predicted[predicted>=0.5]=1;predicted[predicted<0.5]=0
tab1=xtabs(~validation$survived+predicted)
prop.table(tab1)


## larger model
fit=glm(survived~age+sex+factor(pclass)+fare+factor(embarked),family=binomial(),data=train);
summary(fit)
predicted=predict(fit,newdata=validation,type="response")
predicted[predicted>=0.5]=1;predicted[predicted<0.5]=0
tab1=xtabs(~validation$survived+predicted)
prop.table(tab1)


### Calling Rpart library
#install.packages("rpart") should be install by default?
library(rpart)
set.seed(10123456)


## fit small model - ##
tfit1=rpart(factor(survived,levels=0:1,labels=c('died','survived'))~age+factor(sex)+factor(embarked),cp=-0,minsplit=1,minbucket=1,data=train,method='class',maxdepth=3)
plot(tfit1,margin=0.01,uniform=T);  text(tfit1,use.n=T,xpd=T,minlength=6);
pred=predict(tfit1,newdata=validation)
guess=rep(0,nrow(validation))
guess[pred[,2]>0.5]=1
tab=xtabs(~validation$survived+guess)
prop.table(tab)


## fit small model - complex tree ##
tfit2=rpart(factor(survived,levels=0:1,labels=c('died','survived'))~age+factor(sex),data=train,method='class',cp=0)
plot(tfit2,uniform=T,margin=0.01);  text(tfit2,use.n=T,xpd=T,minlength=6);
pred=predict(tfit2,newdata=validation)
guess=rep(0,nrow(validation))
guess[pred[,2]>0.5]=1
tab=xtabs(~validation$survived+guess)
prop.table(tab)


## fit small model - simple tree ##
tfit3=rpart(factor(survived,levels=0:1,labels=c('died','survived'))~age+factor(sex),data=train,method='class',cp=0.01)
plot(tfit3,uniform=T);  text(tfit3,use.n=T,xpd=T,minlength=6);
pred=predict(tfit3,newdata=validation)
guess=rep(0,nrow(validation))
guess[pred[,2]>0.5]=1
tab=xtabs(~validation$survived+guess)
prop.table(tab)


## gini index
f=function(p) {1-p^2-(1-p)^2}
curve(f,from=0,to=1,lwd=3,col='red',xlab='p',ylab='GINI=1-p1^2-p2^2')
f3=function(p1,p2){1-p1^2-p2^2-(1-p1-p2)^2}
p1=seq(0,1,len=40)
p2=p1
vf3=Vectorize(f3)
y=outer(p1,p2,vf3)
library(rgl)
persp3d(p1,p2,y,zlab="GINI", col="blue",lit=F,back='lines',front='lines')
spheres3d(.3333,.3333,1-3*(.3333^2),col='red',radius=0.03)


##################################
y=rep(c("yes","no"),c(10,7))
x=c(28,24,24,24,24,24,26,28,30,30,24,24,24,30,30,30,30)
z=c('a','a','a','b','b','a','a','a','a','a','a','b','a','b','a','b','b')
exd=data.frame(y=y,x=x,z=z,stringsAsFactors = FALSE)
exd=exd[order(x),]
p=prop.table(table(exd$y));gini=p[1]*p[2]

t1=rpart(y~x+z,data=exd,minsplit=1,maxdepth=4,cp=-1,minbucket=1)
par(xpd = TRUE)
plot(t1, compress = TRUE)
text(t1, use.n = TRUE)
exd[order(exd$x,exd$y),]
exd[order(exd$x),]


### 
## maxdepth=3
tfit1=rpart(factor(survived,levels=0:1,labels=c('died','survived'))~age+factor(sex)+factor(embarked),cp=-0,minsplit=1,minbucket=1,data=train,method='class',maxdepth=3)
plot(tfit1,margin=0.01,uniform=T);  text(tfit1,use.n=T,xpd=T,minlength=6);


## maxdepth=30
tfit1a=rpart(factor(survived,levels=0:1,labels=c('died','survived'))~age+factor(sex)+factor(embarked),cp=-0,minsplit=1,minbucket=1,data=train,method='class',maxdepth=30)
plot(tfit1a,margin=0.01,uniform=T);  text(tfit1a,use.n=T,xpd=T,minlength=6);


## minsplit=20
tfit1b=rpart(factor(survived,levels=0:1,labels=c('died','survived'))~age+factor(sex)+factor(embarked),cp=-0,minsplit=20,minbucket=1,data=train,method='class',maxdepth=30)
plot(tfit1b,margin=0.01,uniform=T);  text(tfit1b,use.n=T,xpd=T,minlength=6);


## minbucket=7
tfit1c=rpart(factor(survived,levels=0:1,labels=c('died','survived'))~age+factor(sex)+factor(embarked),cp=-0,minsplit=20,data=train,method='class',maxdepth=30)
plot(tfit1c,margin=0.01,uniform=T);  text(tfit1c,use.n=T,xpd=T,minlength=6);


## cp =0.01
tfit1d=rpart(factor(survived,levels=0:1,labels=c('died','survived'))~age+factor(sex)+factor(embarked),minsplit=20,data=train,method='class',maxdepth=30)
plot(tfit1d,margin=0.01,uniform=T)
text(tfit1d,use.n=T,xpd=T,minlength=6);


## cp =0.0001
tfit1e=rpart(factor(survived,levels=0:1,labels=c('died','survived'))~age+factor(sex)+factor(embarked),minsplit=20,data=train,method='class',maxdepth=30,cp=0.0001)
plot(tfit1e,margin=0.01,uniform=T)
text(tfit1e,use.n=T,xpd=T,minlength=6)


### prune tree
printcp(tfit1a)
plotcp(tfit1a)
pruned=prune(tfit1a,cp=0.092)
plot(pruned)
text(pruned,use.n=T,xpd=T,minlength=6)

