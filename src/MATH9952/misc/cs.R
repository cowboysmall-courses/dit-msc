
library(multcomp)

setwd("~/Workspace/College/DIT/MATH9952/Data")



data = read.csv("cs.csv", header = T)
colnames(data)
attach(data)



plot(pprivate, cs, pch = 20, col = "blue")



fit1 = glm(cs ~ pmultiple + mage + pprivate + offset(ln), family = poisson(), data = data)
summary(fit1)



nd = data.frame(mage = 29, pprivate = 0.22, pmultiple = 0.08, ln = log(100))
p = predict(fit1, newdata = nd, se = T)

ci = c(p$fit - 1.96*p$se, p$fit + 1.96*p$se)
exp(p$fit)
exp(ci)



# nd2 = data.frame(mage = 0, pprivate = 0.1, pmultiple = 0, ln = 0)
# p2 = predict(fit1, newdata = nd2, se = T)
# 
# ci2 = c(p2$fit - 1.96*p2$se, p2$fit + 1.96*p2$se)
# exp(p2$fit)
# exp(ci2)
# 
# 
# 
# nd3 = data.frame(mage = 0, pprivate = 0.1, pmultiple = 0, ln = log(100))
# p3 = predict(fit1, newdata = nd3, se = T)
# 
# ci3 = c(p3$fit - 1.96*p3$se, p3$fit + 1.96*p3$se)
# exp(p3$fit)
# exp(ci3)

L = cbind(0, 0, 0, 0.1)
est = glht(fit1, linfct = L)
summary(est)

c(0.122424 - (1.96*0.003875), 0.122424 + (1.96*0.003875))

coef(est)
confint(est)
