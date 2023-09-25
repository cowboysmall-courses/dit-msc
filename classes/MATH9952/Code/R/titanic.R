
# set.seed(10123456)


source("~/Workspace/College/DIT/MATH9952/Scripts/rpart_functions.R")


# titanic data
titanic = read.csv("~/Workspace/College/DIT/MATH9952/Data/titanic.csv", header = T)


attributes(titanic)$names = tolower(attributes(titanic)$names)
colnames(titanic)


result.labels    = c('died', 'survived')
titanic$result   = factor(titanic$survived, levels = 0:1, labels = result.labels)
titanic$embarked = as.factor(titanic$embarked)
titanic$pclass   = as.factor(titanic$pclass)
titanic$sex      = as.factor(titanic$sex)


include = rbinom(dim(titanic)[1], 1, 2/3)
train   = subset(titanic, include == 1)
val     = subset(titanic, include == 0)


fit0 = fit.buildP(result ~ age + sex + embarked + fare + parch + sibsp + pclass, data = train, cp = -0)
fit.plot(fit0)
fit.plotcp(fit0)


fit1 = fit.prune(fit0, cp = 0.018)
fit.plot(fit1)
fit.plotcp(fit1)


pred = fit.predict(fit1, val)
fit.performance(fit1, pred, val, rev(result.labels))
fit.analysis(fit1, pred, val, rev(result.labels))
