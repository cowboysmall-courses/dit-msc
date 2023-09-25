
library(multcomp)


setwd("~/Workspace/College/DIT/MATH9952/Data")


leprosy = read.csv("leprosy.csv", header = T)
attach(leprosy)
leprosy


fit1 = lm(post_treat ~ factor(drug) * pre_treat, data = leprosy)
model.matrix(fit1)
summary(fit1)
drop1(fit1, test = 'F')

fit2 = lm(post_treat ~ factor(drug) + pre_treat, data = leprosy)
model.matrix(fit2)
summary(fit2)
drop1(fit2, test = 'F')

L = matrix(c(0, 1, -1, 0), nrow = 1)
glh = glht(fit2, linfct = L)
summary(glh, test = Ftest())
