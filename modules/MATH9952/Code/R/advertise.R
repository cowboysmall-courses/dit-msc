setwd("~/Workspace/College/DIT/MATH9952/Data")

data = read.csv("Advertise.csv", header = T)

colnames(data) = c("budget", "ri", "company")
attach(data)
plot(budget, ri, pch = 20, col = 'blue')

fit1 = lm(ri ~ budget, data = data)
abline(reg = fit1, col = 'red', lwd = 2)
summary(fit1)

fit2 = lm(ri ~ budget + I(budget^2), data = data)
fit2
curve(7.05932 + 1.08471*x - 0.00399*x^2, from = 0, to = 200, col = 'darkgreen', lwd = 2, add = T)
summary(fit2)

fit3 = lm(ri ~ budget + I(budget^2) + I(budget^3), data = data)
fit3
curve(8.765e-01 + 1.626e+00*x - 1.327e-02*x^2 + 3.673e-05*x^3, from = 0, to = 200, col = 'orange', lwd = 2, add = T)
summary(fit3)

legend(125, 20, legend = c("Linear Model", "Quadratic Model", "Cubic Model"), lty = 1, col = c('red', 'darkgreen', 'orange'), lwd = 2)
