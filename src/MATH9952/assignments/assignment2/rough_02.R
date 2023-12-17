
# install.packages("gplots")
# install.packages("ggplot2")
# install.packages("gdata")
# install.packages("multcomp")
# install.packages("rmarkdown")

# library(gplots)
# library(ggplot2)
library(gdata)
library(multcomp)

setwd("~/Workspace/College/DIT/MATH9952/Data")

data = read.xls("skincells.xls", header = T)
attach(data)



logcells = logcells[day != 1]
time = time[day != 1]

fit5 = lm(logcells ~ time + I(time^2), data = data)

plot(time, logcells, pch = 20, col = "blue", xlab = "time", ylab = "log cells")

sx = seq(min(time), max(time), len = 200)
sy = fit5$coeff[1] + fit5$coeff[2]*sx + fit5$coeff[3]*sx^2
lines(sx, sy, col = 'green', lwd = 2)



fit_day1 = lm(logcells ~ time, data = data[day == 1, ])
fit_day1

mean(logcells[day == 1])
mean(time[day == 1])


fit_day1 = lm(logcells[day == 1] ~ time[day == 1], data = data)
fit_day2 = lm(logcells[day == 2] ~ time[day == 2], data = data)
fit_day3 = lm(logcells[day == 3] ~ time[day == 3], data = data)
fit_day4 = lm(logcells[day == 4] ~ time[day == 4], data = data)

confint_day1 = data.frame(confint(fit_day1))[1, ]
coef_day1 = data.frame(coef(fit_day1))[1, ]

confint_day1[1]
coef_day1

confint_day2 = data.frame(confint(fit_day2))
coef_day2 = coef(fit_day2)

confint_day2
coef_day2

confint_day3 = data.frame(confint(fit_day3))
coef_day3 = coef(fit_day3)

confint_day3
coef_day3

confint_day4 = data.frame(confint(fit_day4))
coef_day4 = coef(fit_day4)

confint_day4
coef_day4






coef_day1 = data.frame(coef(fit_day1))[1, ]
coef_day2 = data.frame(coef(fit_day2))[1, ]
coef_day3 = data.frame(coef(fit_day3))[1, ]
coef_day4 = data.frame(coef(fit_day4))[1, ]

confint_day1 = data.frame(confint(fit_day1))[1, ]
confint_day2 = data.frame(confint(fit_day2))[1, ]
confint_day3 = data.frame(confint(fit_day3))[1, ]
confint_day4 = data.frame(confint(fit_day4))[1, ]

logcells = vector();
append(logcells, c(coef_day1, coef_day2, coef_day3, coef_day4))

ci_lowers = vector();
append(ci_lowers, c(confint_day1[1], confint_day2[1], confint_day3[1], confint_day4[1]))

ci_uppers = vector();
append(ci_uppers, c(confint_day1[2], confint_day2[2], confint_day3[2], confint_day4[2]))

plotCI(logcells, uiw = ci_uppers, liw = ci_lowers, xlim = c(1, 4), ylim = c(1, 12), main = 'Mean Logcells (With 95% CI)', xlab = 'day', ylab = 'log cells');

plotmeans(logcells ~ day, data = data[day == 1, ])










fit0 = lm(logcells ~ 1, data = data)

# fit1 = lm(logcells ~ time + I(time^2) + factor(day) + factor(day):time + factor(day):I(time^2), data = data)
# drop1(fit1, test = 'F')
# 
# fit2 = update(fit1, ~. - factor(day):I(time^2))
# drop1(fit2, test = 'F')
# 
# fit3 = update(fit2, ~. - factor(day):time)
# drop1(fit3, test = 'F')
# 
# fit4 = update(fit3, ~. - factor(day))
# drop1(fit4, test = 'F')


fit1 = lm(logcells ~ factor(day) * time, data = data)
values = anova(fit1)

values[3, 4]
values[3, 5]

model.matrix(fit1)
summary(fit1)
drop1(fit1, test = 'F')

fit2 = update(fit1, ~. - factor(day):time)
model.matrix(fit2)
summary(fit2)
drop1(fit2, test = 'F')


# step(fit0, scope = list(lower = ~ 1, upper = ~ time + I(time^2) + factor(day) + factor(day):time + factor(day):I(time^2)), direction = "forward", trace = 1)
# step(fit1, scope = list(lower = ~ 1, upper = ~ time + I(time^2) + factor(day) + factor(day):time + factor(day):I(time^2)), direction = "backward", trace = 1)
# step(fit1, scope = list(lower = ~ 1, upper = ~ time + I(time^2) + factor(day) + factor(day):time + factor(day):I(time^2)), direction = "both", trace = 1)

step(fit0, scope = list(lower = ~ 1, upper = ~ time * factor(day)), direction = "forward", trace = 1)
step(fit1, scope = list(lower = ~ 1, upper = ~ time * factor(day)), direction = "backward", trace = 1)
step(fit1, scope = list(lower = ~ 1, upper = ~ time * factor(day)), direction = "both", trace = 1)



model.matrix(fit1)
summary(fit1)
anova(fit1)
confint(fit1)
coef(fit1)

model.matrix(fit2)
summary(fit2)
anova(fit2)
confint(fit2)
coef(fit2)


out = anova(fit1)
out[3, 4]
out[3, 5]


out = anova(fit2)
out[2, 4]
out[2, 5]
out


L = diag(8)[6:8, ]
glh = glht(fit1, linfct = L)
s = summary(glh, test = Ftest())
s
s[1]
s[2]
s[3]
s[4]


L = diag(5)[2:4,]
glh = glht(fit2, linfct = L)
summary(glh, test = Ftest())



L = matrix(c(0, 1, -1, 0, 0), nrow = 1)
glh = glht(fit2, linfct = L)
summary(glh, test = Ftest())

L = matrix(c(0, 0, 1, -1, 0), nrow = 1)
glh = glht(fit2, linfct = L)
summary(glh, test = Ftest())

L = matrix(c(0, 1, 0, -1, 0), nrow = 1)
glh = glht(fit2, linfct = L)
summary(glh, test = Ftest())

L = matrix(c(0, 1, 0, 0, 0), nrow = 1)
glh = glht(fit2, linfct = L)
summary(glh, test = Ftest())

L = matrix(c(0, 0, 1, 0, 0), nrow = 1)
glh = glht(fit2, linfct = L)
summary(glh, test = Ftest())

L = matrix(c(0, 0, 0, 1, 0), nrow = 1)
glh = glht(fit2, linfct = L)
summary(glh, test = Ftest())







# fit3 = update(fit2, ~. + I(time^2))
fit3 = lm(logcells ~ time + I(time^2), data = data)

model.matrix(fit3)
summary(fit3)
anova(fit3)
confint(fit3)
coef(fit3)



plot(time[day == 1], logcells[day == 1], pch = 20, col = 'orange', xlab = "time", ylab = "log cells")
points(time[day == 2], logcells[day == 2], pch = 20, col = 'cyan')
points(time[day == 3], logcells[day == 3], pch = 20, col = 'green')
points(time[day == 4], logcells[day == 4], pch = 20, col = 'yellow')



sx = seq(min(time), max(time), len = 200)

sy = fit3$coeff[1] + fit3$coeff[2]*sx + fit3$coeff[3]*sx^2
lines(sx, sy, col = 'orange', lwd = 2)

sy = (fit3$coeff[1] + fit3$coeff[4]) + fit3$coeff[2]*sx + fit3$coeff[3]*sx^2
lines(sx, sy, col = 'cyan', lwd = 2)

sy = (fit3$coeff[1] + fit3$coeff[5]) + fit3$coeff[2]*sx + fit3$coeff[3]*sx^2
lines(sx, sy, col = 'green', lwd = 2)

sy = (fit3$coeff[1] + fit3$coeff[6]) + fit3$coeff[2]*sx + fit3$coeff[3]*sx^2
lines(sx, sy, col = 'yellow', lwd = 2)






fit1 = lm(logcells ~ factor(day) + time, data = data)
model.matrix(fit1)
values = summary(fit1)
values = anova(fit1)
values

L = matrix(c(0, 1, 0, 0, 0), nrow = 1)
L = matrix(c(0, 0, 1, 0, 0), nrow = 1)
L = matrix(c(0, 0, 0, 1, 0), nrow = 1)
glh = glht(fit1, linfct = L)
summary(glh, test = Ftest())

values$coefficients[2, 4]








data1 = data[day == 1, ]
data2 = data[day == 2, ]
data3 = data[day == 3, ]
data4 = data[day == 4, ]


plot(data1$time, data1$logcells, pch = 20, col = 'blue')
plot(data2$time, data2$logcells, pch = 20, col = 'blue')
plot(data3$time, data3$logcells, pch = 20, col = 'blue')
plot(data4$time, data4$logcells, pch = 20, col = 'blue')



fit1 = lm(logcells ~ time + factor(day) + time:factor(day), data = data)
model.matrix(fit1)
summary(fit1)
anova(fit1)
drop1(fit1, test = 'F')

fit2 = lm(logcells ~ time + factor(day), data = data)
model.matrix(fit2)
summary(fit2)
anova(fit2)
drop1(fit2, test = 'F')

fit3 = lm(logcells ~ time + I(time^2) + factor(day), data = data)
model.matrix(fit3)
summary(fit3)
anova(fit3)

coef(fit1)
coef(fit2)
coef(fit3)


fit = lm(logcells ~ time + factor(day), data = data)
summary(fit)
anova(fit)



plot(time[day == 1], logcells[day == 1], pch = 20, col = 'blue')
points(time[day == 2], logcells[day == 2], pch = 20, col = 'red')
points(time[day == 3], logcells[day == 3], pch = 20, col = 'green')
points(time[day == 4], logcells[day == 4], pch = 20, col = 'yellow')

abline(reg = fit, col = 'red', lwd = 2)

curve(8.3540313 - 4.3804760*x + 0.8758162*x^2, from = 0, to = 3.5, col = 'darkgreen', lwd = 2, add = T)
