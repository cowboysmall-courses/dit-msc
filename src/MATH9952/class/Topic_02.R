##########################################################################
##########################################################################
########   Topic 2: Generalised Linear Models   ########
##########################################################################
##########################################################################
# NB: The path to files below need to be changed to reflect you own
# computing setup. The path statements as shown are those used on a 
# linux operating system
# setwd('~/joe/lectures/DATA')




######## Oring Data  ######################################################
oring = read.csv("~/Workspace/College/DIT/MATH9952/Data/csv/oring.csv", header = T)
attach(oring)
pfailures = nFailures / 6

plot(Temperature, pfailures, pch = 20, col = 'blue')

fit0 = lm(pfailures ~ Temperature)
lines(Temperature, fit0$fitted, col = 'red', lwd = 2)


##
#### Using NR to fit mode to Oring data
##
x = Temperature
y = nFailures

beta0 = 1
beta1 = 0
beta  = matrix(c(beta0, beta1), nrow = 2)

## iterations here ###

eta    = (beta[1, 1] + beta[2, 1] * x)
score1 = sum(y - (6 * exp(eta)) / (1 + exp(eta)))
score2 = sum(y * x - (6 * x * exp(eta)) / (1 + exp(eta)))

h1 = sum((6 * exp(eta)^2)       / ((1 + exp(eta))^2) - (6 * exp(eta)) / (1 + exp(eta)))
h2 = sum((6 * x^2 * exp(eta)^2) / ((1 + exp(eta))^2) - (6 * x^2 * exp(eta)) / (1 + exp(eta)))
h3 = sum((6 * x *exp(eta)^2)    / ((1 + exp(eta))^2) - (6 * x * exp(eta)) / (1 + exp(eta)))

u = matrix(c(score1, score2), nrow = 2)
h = matrix(c(h1, h3, h3, h2), nrow = 2, byrow = T)

beta
betanew = beta - solve(h) %*% u
beta = betanew

results = data.frame(beta = beta, score = u, hessian = h)
results

### end iterations





### fitting model using glm()
m = matrix(c(nFailures, 6 - nFailures), ncol = 2, byrow = F)
fit1 = glm(m ~ Temperature, family = binomial())
summary(fit1)

sx   = seq(min(Temperature), max(Temperature), len = 100)
seta = fit1$coeff[1] + fit1$coeff[2] * sx;
sy   = exp(seta) / (1 + exp(seta))
lines(sx, sy, col = 'green', lwd = 3)


######## Neuralgia Data  ######################################################
neuralgia = read.table("~/Workspace/College/DIT/MATH9952/Data/txt/neuralgia.txt", header = T, sep = '')

fit2 = glm(Pain ~ Age + Duration + factor(Sex) + factor(Treatment), family = binomial(), data = neuralgia)
summary(fit2)
logLik(fit2)


## null deviance model - defined as model with only intercept
fit2t = glm(Pain ~ 1, family = binomial(), data = neuralgia)


## significance of each term included?
drop1(fit2, test = 'LRT')


### get CI and customised test for B V P.
L  = matrix(c(0, 0, 0, 0, 1, -1), nrow = 1)
LB = L %*% matrix(fit2$coeff, ncol = 1)


## Test null hypothesis of no difference
LV = L %*% vcov(fit2) %*% t(L)
waldtest = LB %*% solve(LV) %*% t(LB);
pvalue = 1 - pchisq(waldtest,dim(L)[1])
print(c(waldtest, pvalue))


## Get CI on odds-ratio scale
CI = c(LB - 1.96 * sqrt(L %*% vcov(fit2) %*% t(L)), LB + 1.96 * sqrt(L %*% vcov(fit2) %*% t(L)))
exp(CI)







######## Claims Data  ######################################################
claims = read.table("~/Workspace/College/DIT/MATH9952/Data/txt/claims.txt", header = T, sep = '')

fit3 = glm(claims ~ age, family = poisson(), data = claims)

summary(fit3);
logLik(fit3)







######## Epilepsy Data  ######################################################
epilepsy = read.table("~/Workspace/College/DIT/MATH9952/Data/txt/epilepsy.txt", header = T, sep = ',')

fit4 = glm(Attacks ~ factor(treatment) + offset(log(Time.)), family = poisson(), data = epilepsy)

summary(fit4)
logLik(fit4)

#### see what happens without offset
fit4a = glm(Attacks ~ factor(treatment), family = poisson(), data = epilepsy)

summary(fit4a)
logLik(fit4a)
