
library(survival)




whas = read.csv("~/Workspace/College/DIT/MATH9952/Data/whas.csv", header = T)




attach(whas)
colnames(whas)




km1 = survfit(Surv(lenfol, fstat) ~ 1, conf.type = 'plain', data = whas)
plot(km1)


km2 = survfit(Surv(lenfol, fstat) ~ factor(gender), conf.type = 'plain', data = whas)
plot(km2, col = c('red', 'blue'))
survdiff(Surv(lenfol, fstat) ~ factor(gender), data = whas)


km3 = survfit(Surv(lenfol, fstat) ~ factor(bmi_group), conf.type = 'plain', data = whas)
plot(km3, col = c('red', 'blue', 'green', 'purple'))
survdiff(Surv(lenfol, fstat) ~ factor(bmi_group), data = whas)


km4 = survfit(Surv(lenfol, fstat) ~ factor(miord), conf.type = 'plain', data = whas)
plot(km4, col = c('red', 'blue'))
survdiff(Surv(lenfol, fstat) ~ factor(miord), data = whas)




###############################################################################################

fit1 = coxph(Surv(lenfol, fstat) ~ factor(miord), data = whas)
summary(fit1)


fit2 = coxph(Surv(lenfol, fstat) ~ factor(gender), data = whas)
summary(fit2)


fit3 = coxph(Surv(lenfol, fstat) ~ factor(gender) + age, data = whas)
summary(fit3)


fit4 = coxph(Surv(lenfol, fstat) ~ age, data = whas)
summary(fit4)




fita = coxph(Surv(lenfol, fstat) ~ age + factor(gender) + hr + factor(bmi_group) + cvd + chf + factor(miord) + los, data = whas)
drop1(fita, test = 'Chisq')

fitb = update(fita, ~. - cvd)
drop1(fitb, test = 'Chisq')

fitc = update(fitb, ~. - los)
drop1(fitc, test = 'Chisq')

fitd = update(fitc, ~. - factor(miord))
drop1(fitd, test = 'Chisq')




fitf = coxph(Surv(lenfol, fstat) ~ age + factor(gender) + hr + factor(bmi_group) + chf, data = whas)
summary(fitf)





###############################################################################################

detach(whas)


