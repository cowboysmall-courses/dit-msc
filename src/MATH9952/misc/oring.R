
setwd("~/Workspace/College/DIT/MATH9952/Data")

oring = read.csv("oring.csv", header = T)
attach(oring)

pFailures = nFailures / 6

fit0 = lm(pFailures ~ Temperature)

plot(Temperature, pFailures, pch = 20, col = "blue")
lines(Temperature, fit0$fitted, col = "red", lwd = 2)

x = Temperature
y = nFailures

nFailures

beta0  = 1
beta1  = 0
beta   = matrix(c(beta0, beta1), nrow = 2)


# iterations start...

eta     = (beta[1, 1] + beta[2, 1] * x)

score1  = sum(y - ((6 * exp(eta)) / (1 + exp(eta))))
score2  = sum((y * x) - ((6 * x * exp(eta)) / (1 + exp(eta))))

h1      = sum( (6 * exp(eta)^2) / (1 + exp(eta))^2 - ((6 * exp(eta)) / (1 + exp(eta))))
h2      = sum( (6 * x^2 * exp(eta)^2) / (1 + exp(eta))^2 - ((6 * x^2 * exp(eta)) / (1 + exp(eta))))
h3      = sum( (6 * x * exp(eta)^2) / (1 + exp(eta))^2 - ((6 * x * exp(eta)) / (1 + exp(eta))))

u       = matrix(c(score1, score2), nrow = 2)
h       = matrix(c(h1, h3, h3, h2), nrow = 2, byrow = T)
betanew = beta - solve(h) %*% u
beta    = betanew

result  = data.frame(beta = beta, score = u, hessian = h) 
result

# iterations end...

m    = matrix(c(nFailures, 6 - nFailures), ncol = 2, byrow = F)
fit1 = glm(m ~ Temperature, family = binomial())
summary(fit1)
result

sx   = seq(min(Temperature), max(Temperature), len = 100)
seta = fit1$coeff[1] + fit1$coeff[2] * sx 
sy   = exp(seta) / (1 + exp(seta))

lines(sx, sy, col = 'green', lwd = 3)
