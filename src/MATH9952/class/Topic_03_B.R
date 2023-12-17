
data=read.csv("/home/joe/joe/lectures/DT9209\ MSc\ Statistics/Data/titanic.csv",header=T)

attributes(data)$names=tolower(attributes(data)$names)


### reproducible random stream 
set.seed(10123456)
include=rbinom(dim(data)[1],1,2/3)
train=subset(data,include==1)
validation=subset(data,include==0)
validation$survived=factor(validation$survived,levels=0:1,labels=c('died','survived'))


### Calling Rpart library
library(rpart)


## fit model - ##
tfit1=rpart(factor(survived,levels=0:1,labels=c('died','survived'))~age+factor(sex)+factor(embarked)+fare+parch+sibsp+factor(pclass),cp=-0,data=train,method='class')


# plot tree
plot(tfit1,margin=0.01,uniform=T)
text(tfit1,use.n=T,xpd=T,minlength=6)


### prune tree?
plotcp(tfit1)
printcp(tfit1)
tfit1=prune(tfit1,cp=0.018)


# plot pruned tree
plot(tfit1,margin=0.01,uniform=T)
text(tfit1,use.n=T,xpd=T,minlength=6)


## now use validation dataset to assess tree performance
pred=predict(tfit1,newdata=validation)
guess=rep('p_survived',nrow(validation))
guess[pred[,1]>0.5]='p_died'
table(guess)


## confusion matrix
tab=xtabs(~validation$survived+guess)
prop.table(tab)


## sensitivity & specificity
prop.table(tab,1)


## go back and redo for decision threshold=0.85
## what if we view the sesnitivity V specificity relationship 
## in a systematic fashion?
# We can plot ROC curve using the ROCR package
install.packages("ROCR")
library(ROCR)

p.hat = predict(tfit1, newdata = validation)
p.scores <- prediction(p.hat[,1], validation$survived, label.ordering = c('survived','died'))
## note need to order negative cases first! - hence use of 
## label.ordering() option here.
p.perf <- performance(p.scores, "tpr", "fpr")


# Plot the ROC curve
plot(p.perf, col = "blue", lwd = 2,xlab='1-Specificity (FPR)',ylab='Sensitivity (TPR)')
abline(a=0,b=1,col='red',lty=3,lwd=2)


#AUC
p.auc <- performance(p.scores, "auc")
text(0.1,1, round(p.auc@y.values[[1]],3),col='red')

## note the way we need to extract elements of an s4 class 
##object


