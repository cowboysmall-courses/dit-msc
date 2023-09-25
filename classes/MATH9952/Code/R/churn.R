
# install.packages("ROCR")

library(rpart)
library(ROCR)

setwd("~/Workspace/College/DIT/MATH9952/Data")

churn = read.csv("churn.csv", header = T)
attach(churn)
colnames(churn)
table(churn$customer_churn)





response = rep(1, 300)
response[customer_churn == 'No'] = 0

fit1 = glm(response ~ tenure + monthlycharges + factor(onlinebackup) + factor(multiplelines), data = churn)
summary(fit1)
drop1(fit1, test = 'LRT')

fit2 = update(fit1, . ~ . - factor(multiplelines))
summary(fit2)
drop1(fit2, test = 'LRT')

fit3 = update(fit2, . ~ . - factor(onlinebackup))
summary(fit3)
drop1(fit3, test = 'LRT')

p = predict(fit3, newdata = data.frame(tenure = 12, monthlycharges = 70), se.fit = T)


cip = c(p$fit - (1.96 * p$se.fit), p$fit + (1.96 * p$se.fit))
cip = exp(cip) / (1 + exp(cip))



# tfit = rpart(factor(customer_churn) ~ factor(gender) + tenure, data = churn)
# tfit = rpart(factor(customer_churn) ~ factor(gender) + tenure, cp = 0.09, data = churn)
# tfit = rpart(factor(customer_churn) ~ factor(gender) + tenure, cp = 0.0001, minsplit = 1, minbucket = 1, data = churn)

# plot(tfit, margin = 0.01, uniform = T)
# text(tfit, use.n = T, minlength = 6)



# set.seed(27041970)

include = rbinom(dim(churn)[1], 1, 2/3)

train      = subset(churn, include == 1)
validation = subset(churn, include == 0)

tfit1 = rpart(
  factor(customer_churn) ~ factor(gender) + factor(seniorcitizen) + factor(partner) + factor(dependents) + tenure + factor(phoneservice) + factor(multiplelines) + factor(internetservice) 
    + factor(onlinesecurity) + factor(onlinebackup) + factor(deviceprotection) + factor(techsupport) + factor(streamingtv) + factor(streamingmovies) + factor(paperlessbilling) 
    + factor(paymentmethod) + monthlycharges + totalcharges, 
  cp = -0, 
  data = train, 
  # minbucket = 1, 
  # minsplit = 2, 
  method = "class"
)

plot(tfit1, margin = 0.01, uniform = T)
text(tfit1, use.n = T, minlength = 6)

plotcp(tfit1)
printcp(tfit1)

tfit1 = prune(tfit1, cp = 0.025)

plot(tfit1, margin = 0.01, uniform = T)
text(tfit1, use.n = T, minlength = 6)

pred  = predict(tfit1, newdata = validation)

guess = rep("p_churned", nrow(validation))
guess[pred[, 1] > 0.7] = "p_no_churn"
table(guess)

tab = xtabs(~ validation$customer_churn + guess)
prop.table(tab)
prop.table(tab, 1)

p.hat    = predict(tfit1, newdata = validation)
p.scores = prediction(p.hat[, 1], validation$customer_churn, label.ordering = c("Yes", "No"))
p.perf   = performance(p.scores, "tpr", "fpr")
p.auc    = performance(p.scores, "auc")

plot(p.perf, col = "blue", lwd = 2, xlab = "1 - Specificity (FPR)", ylab = "Sensitivity (TPR)")
abline(a = 0, b = 1, col = "red", lty = 3, lwd = 2)
text(0.1, 1, round(p.auc@y.values[[1]], 3), col = "red")


