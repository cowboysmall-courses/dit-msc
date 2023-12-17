
# setwd('~/joe/lectures/DATA')

library(survival)




#### Question 1 #############

steroid = c(-2, -2, 0, 0, 1, 2, 4)
heart_rate = c(67, 70, 71, 72, 75, 75, 74)
model = lm(heart_rate ~ steroid + I(steroid^2))

#b(i)
X_matrix = model.matrix(model)
dimnames(X_matrix) = list(rep("", dim(X_matrix)[1]), rep("", dim(X_matrix)[2]))
attr(X_matrix, "assign") = NULL
X_matrix

# t(X_matrix)
# t(X_matrix) %*% X_matrix
# solve(t(X_matrix) %*% X_matrix)

#b(iii) NB: Manual calculation will only be accurate to first decimal place.
p = predict(model, newdata = data.frame(steroid = 3))
p

predict(model, newdata = data.frame(steroid = 3), interval = 'confidence')
predict(model, newdata = data.frame(steroid = 3), interval = 'confidence')[2:3]

summary(model)

t = qt(0.975, df = 4)
t

L = cbind(1, 3, 9)
L
t(L)

Lbeta = L %*% model$coeff
Lbeta

vc = vcov(model)
se = sqrt(L %*% vc %*% t(L))
c(Lbeta - t * se, Lbeta + t * se)

XX = solve(t(X_matrix) %*% X_matrix)
XX
21956 * XX

ss = sum(model$residuals^2) / model$df.residual
VC = ss * XX
se = sqrt(L %*% VC %*% t(L))
c(Lbeta - t * se, Lbeta + t * se)







#### Question 2 #############

cs = read.csv(file = "~/Workspace/College/DIT/MATH9952/Data/csv/cs.csv")

cs$pprivate = cs$private / cs$n
cs$pmultiple = cs$multiple / cs$n
cs$ln = log(cs$n)
fit = glm(cs ~ pmultiple + mage + pprivate + offset(ln), family = poisson(), data = cs)
summary(fit)

# c(ii)
nd = data.frame(ln = log(100), mage = 29, pprivate = 0.22, pmultiple = 0.08)
predict(fit, newdata = nd, type = 'response')

fit$coeff
exp(log(100) + cbind(1, 0.08, 29, 0.22) %*% fit$coeff)
100 * exp(cbind(1, 0.08, 29, 0.22) %*% fit$coeff)


# c(iii)
L = cbind(0, 0, 0, 0.1)
Lbeta = L %*% fit$coeff
se = sqrt(L %*% vcov(fit) %*% t(L))

# effect on c-section rate
exp(Lbeta) # i.e. increase of 13%
exp(c(Lbeta - 1.96 * se, Lbeta + 1.96 * se)) # 95% CI for this increase i.e.  12.2 to 13.8 %







##### question 5 ######
time  = c(1, 21, 23, 42, 46, 55, 83, 2, 7, 57, 71, 154, 361)
event = c(0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0)
type  = rep(c('A', 'B'), c(7, 6))
mech  = data.frame(time = time, event = event, type = type)


# a(i)
fit = survfit(Surv(time, event) ~ factor(type), data = mech)
summary(fit)
plot(fit, col = c('blue', 'red'))

# a(ii)
test = survdiff(Surv(time, event) ~ factor(type), data = mech)
test

### cox models
churn = read.csv(file = "~/Workspace/College/DIT/MATH9952/Data/csv/time_to_churn.csv")

colnames(churn) = tolower(colnames(churn))

churn$status = rep(0, dim(churn)[1])
churn$status[churn$churn == 'Yes'] = 1

churn = churn[complete.cases(churn), ]

attach(churn)

fit = coxph(Surv(tenure, status) ~ factor(gender) + monthlycharges + factor(paperlessbilling), data = churn)
summary(fit)

# b(ii)
L = cbind(1, 0, 0)
Lbeta = L %*% fit$coeff
se = sqrt(L %*% vcov(fit) %*% t(L))
exp(Lbeta) # hazard ratio
exp(c(Lbeta - 1.96 * se, Lbeta + 1.96 * se)) # 95% CI for this hazard ratio
