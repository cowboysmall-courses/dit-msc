##########################################################################
##########################################################################
########   Topic 1: Independent data  Linear Regression MNodels   ########
##########################################################################
##########################################################################
# NB: The path to files below need to be changed to reflect you own
# computing setup. The path statements as shown are those used on a 
# linux operating system
# setwd("~/Workspace/College/DIT/MATH9952/Data/")







######## LDL model  ######################################################

# read in LDL data in dataframe called ldldata
ldldata = read.csv("~/Workspace/College/DIT/MATH9952/Data/csv/ldldata.csv", header = T)

# define y & x vectors
y = ldldata[, 2]
x = ldldata[, 1]

# plot data
plot(x, y, pch = 20, cex = 2, col = 'blue', xlab = 'Weight Kg', ylab = 'LDL mg/dcl')

# fit simple linear regression model using lm function
fit1 = lm(y ~ x)
summary(fit1)

# plot ls line on scatter plot
lines(x, fit1$fitted, col = 'red', lwd = 2)

# define Y and X matrices and get same result using matrix operations 
Y = matrix(y, ncol = 1)
X = matrix(c(rep(1, 15), x), ncol = 2, byrow = F)

bhat = solve(t(X) %*% X) %*% t(X) %*% Y
bhat







######## Dose-response data ##############################################
### dose-response
dr = read.table("~/Workspace/College/DIT/MATH9952/Data/txt/doseresponse.txt", header = T, sep = '')
dr

x  = dr[, 1]
x2 = x^2
y  = dr[, 2]

plot(x, y, main = 'Dose Response Data', xlab = 'Dose', ylab = 'Response', pch = 20, cex = 1.5, col = 'blue')

# fit quadratic regression model using the lm() function
fit2 = lm(y ~ x + x2)
summary(fit2)

# plot ls curve on scatter plot
sx = seq(min(x), max(x), len = 200)
sy = fit2$coeff[1] + fit2$coeff[2] * sx + fit2$coeff[3] * sx^2
lines(sx, sy, col = 'red', lwd = 2)

# define Y and X matrices and get same result using matrix operations 
Y = matrix(y, ncol = 1)
X = matrix(c(rep(1, 11), x, x2), ncol = 3, byrow = F)

bhat = solve(t(X) %*% X) %*% t(X) %*% Y
bhat






######## Breadwrapper Data ###############################################
bw = read.table("~/Workspace/College/DIT/MATH9952/Data/txt/breadwrapper.txt", header = T, sep = '')
bw

# install.packages("rgl") needs to be done just once on a pc
library(rgl)

y  = bw[, 1]
x1 = bw[, 2]
x2 = bw[, 4]

plot3d(x1, x2, y, col = 'red', size = 1, type = 's', xlab = 'Temperature', ylab='Polyethylene',zlab='Seal Strength',par3d(mouseMode=c("trackball")))

fit3 = lm(y ~ x1 + x2)

x_1 = seq(min(x1), max(x1), len = 20)
x_2 = seq(min(x2), max(x2), len = 20)
z = matrix(0, ncol = length(x_2), nrow = length(x_1))

f = function(x_1, x_2) { fit3$coeff[1] + fit3$coeff[2] * x_1 + fit3$coeff[3] * x_2 }

f = Vectorize(f)
z = outer(x_1, x_2, f)

surface3d(x_1, x_2, z, col = "blue", add = T, front = "lines", back = "lines", box = F, axes = F)








#################### General Linear Hypotheses
# install.packages('multcomp')
library(multcomp)

bw = read.table("~/Workspace/College/DIT/MATH9952/Data/txt/breadwrapper.txt", header = T, sep = '')
bw

fit_bw = lm(Seal_Strength ~ sealtemp + polyethylene, data = bw)
summary(fit_bw)
model.matrix(fit_bw)

L1 = diag(3)[-1, ]
L1

glh1 = glht(fit_bw, linfct = L1)
summary(glh1, test = Ftest())

L2 = cbind(0, 1, 0)
L2

glh2 = glht(fit_bw, linfct = L2)
summary(glh2, test = Ftest())




B  = fit_bw$coeff
B

X  = model.matrix(fit_bw)
dimnames(X) = list(rep("", dim(X)[1]), rep("", dim(X)[2]))
attr(X, "assign") = NULL

XX = solve(t(X) %*% X)
ss = sum(fit_bw$residuals^2) / fit_bw$df.residual


se = ss * XX

t1 = B[1] / sqrt(se[1, 1])
t2 = B[2] / sqrt(se[2, 2])
t3 = B[3] / sqrt(se[3, 3])

c(t1, t2, t3)

tt = L2 %*% B / sqrt(ss * (L2 %*% XX %*% t(L2)))
tt

Y  = bw[, 1]
BB = XX %*% t(X) %*% Y
BB

SL = (t(L1 %*% B) %*% t(L1 %*% XX %*% t(L1)) %*% (L1 %*% B))
SL

F1 = SL / (nrow(L1) * ss)
F1

SL = (t(L2 %*% B) %*% solve(L2 %*% XX %*% t(L2)) %*% (L2 %*% B))
SL

F2 = SL / (nrow(L2) * ss)
F2


pf(  5.51, 1, 17, lower.tail = F)
pf(150.13, 3, 17, lower.tail = F)
pf(  5.11, 2, 17, lower.tail = F)
pf( 14.23, 1, 17, lower.tail = F)
pf(  4.82, 1, 17, lower.tail = F)





######## Fitting different degree polynmials to doseresponse data #########
dr = read.table("~/Workspace/College/DIT/MATH9952/Data/txt/doseresponse.txt", header = T, sep = '')

# quadratic model
summary(lm(activity ~ dose + I(dose^2), data = dr))

# source("anovatab.R")
# anovatab(lm(activity~dose+I(dose^2),data=dr))

# cubic model
summary(lm(activity ~ dose + I(dose^2) + I(dose^3), data = dr))

# quartic model
summary(lm(activity ~ dose + I(dose^2) + I(dose^3) + I(dose^4), data = dr))

# quintic model
summary(lm(activity ~ dose + I(dose^2) + I(dose^3) + I(dose^4) + I(dose^5), data = dr))







######## Example of model slection techniques in R using the Fitness data #
fitness = read.table("~/Workspace/College/DIT/MATH9952/Data/txt/fitness.txt", header = T, sep = '')

## forward selection
fitness_fit  = lm(Oxygen ~ 1, data = fitness)
step_fitness = step(fitness_fit, direction = "forward", scope = list(lower = ~ 1, upper = ~ Age + Weight + RunTime + RestPulse + RunPulse + MaxPulse), trace = 1)

## backward selection
fitness_fit  = lm(Oxygen ~ Age + Weight + RunTime + RestPulse + RunPulse + MaxPulse, data = fitness)
step_fitness = step(fitness_fit, direction = "backward", scope = list(lower = ~ 1, upper = ~ Age + Weight + RunTime + RestPulse + RunPulse + MaxPulse), trace = 1)

## stepwise selection
fitness_fit  = lm(Oxygen ~ 1, data = fitness)
step_fitness = step(fitness_fit, direction = "both", scope = list(lower = ~ 1, upper = ~ Age + Weight + RunTime + RestPulse + RunPulse + MaxPulse), trace = 1)








######## Clinical data ######################################################
#### Clinical data
response = c(91, 97, 104, 112, 115, 114, 119, 116, 115)
treat    = c(rep(c(1, 2, 3), rep(3, 3)))

fit4 = lm(response ~ factor(treat))
summary(fit4)

X = model.matrix(fit4)
Y = response
Z = solve(t(X) %*% X) %*% t(X) 
B = Z %*% Y

drop1(fit4, test = 'F')

# install.packages('multcomp')
library(multcomp)

L1 = diag(3)[-1, ]
L1

glh1 = glht(fit4, linfct = L1)
summary(glh1, test = Ftest())



L2 = matrix(c(0, 1, -1), nrow = 1)
L2

glh2 = glht(fit4, linfct = L2)
summary(glh2, test = Ftest())

confint(glh2)



L3 = matrix(c(0, 1, 0), nrow = 1)
L3

glh3 = glht(fit4, linfct = L3)
summary(glh3, test = Ftest())

L4 = matrix(c(0, 0, 1), nrow = 1)
L4

glh4 = glht(fit4, linfct = L4)
summary(glh4, test = Ftest())



La = matrix(c(1, 0, 0), nrow = 1)
Lb = matrix(c(1, 1, 0), nrow = 1)
Lc = matrix(c(1, 0, 1), nrow = 1)

La %*% B
Lb %*% B
Lc %*% B


# the L1 test can be done manually via:
glhm = t(L1 %*% fit4$coeff) %*% solve(L1 %*% vcov(fit4) %*% t(L1)) %*% (L1 %*% fit4$coef) / dim(L1)[1]
p_value = 1 - pf(glhm, 2, 6)
p_value






######## Turkey Data    #####################################################
turkey = read.table("~/Workspace/College/DIT/MATH9952/Data/txt/turkey.txt", header = T, sep = '')

fit5 = lm(weight ~ factor(feed) + age, data = turkey)
summary(fit5)
anova(fit5)
model.matrix(fit5)

### interaction model
fit5a = lm(weight ~ factor(feed) * age, data = turkey)
model.matrix(fit5a)
summary(fit5a)
anova(fit5a)


