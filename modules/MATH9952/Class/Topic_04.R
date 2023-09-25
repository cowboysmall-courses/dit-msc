time=c(4,18,27,45,40,53,180,6,13,37,71,122,222)
event=c(0,0,0,01,0,1,1,0,1,0,1,0,0)
type=c(rep('X',7),rep('M',6))
machines=data.frame(time=time,event=event,type=type)
machines=machines[order(machines$time),]
####
library(survival)
km1=survfit(Surv(time,event)~1,conf.type='plain',data=machines)
summary(km1)
plot(km1)


km2=survfit(Surv(time,event)~type,data=machines)
summary(km2)
plot(km2)
lrtest=survdiff(Surv(time,event)~type,data=machines)
lrtest
##### 
aml
fitleukemia <- survfit(Surv(time, status) ~ 1,conf.type='plain', data = aml) 
plot(fitleukemia) 
summary(fitleukemia)

fitleukemia2 <- survfit(Surv(time, status) ~ x, data = aml) 
plot(fitleukemia2,col=c('blue','red')) 
survdiff(Surv(time,status)~x,data=aml)
