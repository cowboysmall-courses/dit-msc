
# install and load required libraries

# install.packages("gdata")
# install.packages("multcomp")

library(gdata)
library(multcomp)


# set working directory

setwd("~/Workspace/College/DIT/MATH9952/Data")


# read in the data

data = read.xls("skincells.xls", header = T)
attach(data)


# split data by day and fit model for each day

fit_day1 = lm(logcells[day == 1] ~ time[day == 1], data = data)
fit_day2 = lm(logcells[day == 2] ~ time[day == 2], data = data)
fit_day3 = lm(logcells[day == 3] ~ time[day == 3], data = data)
fit_day4 = lm(logcells[day == 4] ~ time[day == 4], data = data)

coef(fit_day1)
coef(fit_day2)
coef(fit_day3)
coef(fit_day4)

confint(fit_day1)
confint(fit_day2)
confint(fit_day3)
confint(fit_day4)


# plot data by day

plot(time[day == 1], logcells[day == 1], pch = 20, col = 'orange', xlab = "time", ylab = "log cells")
points(time[day == 2], logcells[day == 2], pch = 20, col = 'cyan')
points(time[day == 3], logcells[day == 3], pch = 20, col = 'green')
points(time[day == 4], logcells[day == 4], pch = 20, col = 'yellow')
legend(1.5, 12, legend = c("day 1", "day 2", "day 3", "day 4"), fill = c("orange", "cyan", "green", "yellow"), horiz = TRUE)


# fit full interraction model acroass all days and test

fit1 = lm(logcells ~ factor(day) * time, data = data)
summary(fit1)
anova(fit1)
drop1(fit1, test = 'F')


# drop interraction parameter and re-test

fit2 = update(fit1, ~. - factor(day):time)
summary(fit2)
anova(fit2)
drop1(fit2, test = 'F')


# drop day parameter and re-test

fit3 = update(fit2, ~. - factor(day))
summary(fit3)
anova(fit3)
drop1(fit3, test = 'F')


# fit empty model for test purposes

fit0 = lm(logcells ~ 1, data = data)


# test the fitted model by dropping parameters in all three directions - forward, backwards, and both

step(fit0, scope = list(lower = ~ 1, upper = ~ time * factor(day)), direction = "forward", trace = 1)
step(fit1, scope = list(lower = ~ 1, upper = ~ time * factor(day)), direction = "backward", trace = 1)
step(fit1, scope = list(lower = ~ 1, upper = ~ time * factor(day)), direction = "both", trace = 1)


# test days 2, 3, and 4 are equal to day 1

L = diag(5)[2:4,]
glh = glht(fit2, linfct = L)
summary(glh, test = Ftest())

# test day 2 is equal to day 3

L = matrix(c(0, 1, -1, 0, 0), nrow = 1)
glh = glht(fit2, linfct = L)
summary(glh, test = Ftest())

# test day 3 is equal to day 4

L = matrix(c(0, 0, 1, -1, 0), nrow = 1)
glh = glht(fit2, linfct = L)
summary(glh, test = Ftest())

# test day 2 is equal to day 4

L = matrix(c(0, 1, 0, -1, 0), nrow = 1)
glh = glht(fit2, linfct = L)
summary(glh, test = Ftest())

# test days 2 is equal to day 1

L = matrix(c(0, 1, 0, 0, 0), nrow = 1)
glh = glht(fit2, linfct = L)
summary(glh, test = Ftest())

# test days 3 is equal to day 1

L = matrix(c(0, 0, 1, 0, 0), nrow = 1)
glh = glht(fit2, linfct = L)
summary(glh, test = Ftest())

# test days 4 is equal to day 1

L = matrix(c(0, 0, 0, 1, 0), nrow = 1)
glh = glht(fit2, linfct = L)
summary(glh, test = Ftest())


