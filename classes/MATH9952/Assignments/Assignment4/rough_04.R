
set.seed(27047007)


source("~/Workspace/College/DIT/MATH9952/Scripts/rpart_functions.R")


retention          = read.csv("~/Workspace/College/DIT/MATH9952/Data/students_retention_full.csv", header = T)
outcome            = c('failed', 'passed')
retention$result   = factor(retention$overall6, levels = 0:1, labels = outcome)
retention$overall6 = NULL
retention$id       = NULL


attach(retention)


include = rbinom(dim(retention)[1], 1, 2/3)
train   = subset(retention, include == 1)
val     = subset(retention, include == 0)


fit0 = fit.buildP(result ~ ., data = train)
fit.plot(fit0)
fit.plotcp(fit0)

prediction = fit.predict(fit0, val)
fit.performance(fit0, prediction, val, rev(outcome))
fit.analysis(fit0, prediction, val, rev(outcome))



fit0 = fit.build(result ~ ., data = train)
fit.plot(fit0)
fit.plotcp(fit0)

pred = fit.predict(fit0, val)
fit.performance(fit0, pred, val, rev(outcome))
fit.analysis(fit0, pred, val, rev(outcome))



fit1 = fit.buildP(result ~ ., data = train)
fit.plot(fit1)
fit.plotcp(fit1)

prediction = fit.predict(fit1, val)
fit.performance(fit1, prediction, val, rev(outcome))
fit.analysis(fit1, prediction, val, rev(outcome))



fit2 = fit.buildX(result ~ ., data = train)
fit.plot(fit2)
fit.plotcp(fit2)

pred = fit.predict(fit2, val)
fit.performance(fit2, pred, val, rev(outcome))
fit.analysis(fit2, pred, val, rev(outcome))



fit3 = fit.prune(fit2, cp = 0.0172414)
fit.plot(fit3)
fit.plotcp(fit3)

prediction = fit.predict(fit3, val)
fit.performance(fit3, prediction, val, rev(outcome))
fit.analysis(fit3, prediction, val, rev(outcome))









gini.score    = function(p1, p2) { 1 - p1^2 - p2^2 }
entropy.score = function(p1, p2) { -((p1 * log(p1)) + (p2 * log(p2))) }

imp.calculate = function(split1, split2, imp.score) {
  
  length1 = nrow(split1)
  length2 = nrow(split2)
  total   = length1 + length2
  
  prob1   = prop.table(table(split1$result))
  imp1    = (length1 / total) * imp.score(prob1[1], prob1[2])
  
  prob2   = prop.table(table(split2$result))
  imp2    = (length2 / total) * imp.score(prob2[1], prob2[2])
  
  imp1 + imp2
}



prob0   = prop.table(table(train$result))
imp0    = gini.score(prob0[1], prob0[2])
# imp0    = entropy.score(prob0[1], prob0[2])



points  = sort(unique(train$lcpoints))

# maximum = 0.0
minimum = 1.0
point   = 0

for (p in points) {
  
  # v = imp0 - imp.calculate(train[ train$lcpoints  < p, ], train[ train$lcpoints >= p, ], entropy.score)
  # v = imp0 - imp.calculate(train[ train$lcpoints  < p, ], train[ train$lcpoints >= p, ], gini.score)
  v = imp.calculate(train[ train$lcpoints  < p, ], train[ train$lcpoints >= p, ], gini.score)
  # if (is.finite(v) && v < maximum) {
  if (is.finite(v) && v < minimum) {
    
    # maximum = v
    minimum = v
    point   = p
  }
}

# c(point, maximum)
c(point, minimum)



# gini.calculate(train[ train$lcpoints  < 215, ], train[ train$lcpoints >= 215, ])
# gini.calculate(train[ train$lcpoints  < 340, ], train[ train$lcpoints >= 340, ])
# gini.calculate(train[ train$lcpoints  < 405, ], train[ train$lcpoints >= 405, ])



detach(retention)

