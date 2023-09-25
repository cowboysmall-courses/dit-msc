
library(rpart)

f1 = function(p) { 1 - p^2 - (1 - p)^2 }
f2 = function(p1, p2) { 1 - p1^2 - p2^2 - (1 - p1 - p2)^2 }

x = rep(c("yes", "no"), c(10, 7))
y = c(28, 24, 24, 24, 24, 24, 26, 28, 30, 30, 24, 24, 24, 30, 30, 30, 30)
z = c('a', 'a', 'a', 'b', 'b', 'a', 'a', 'a', 'a', 'a', 'a', 'b', 'a', 'b', 'a', 'b', 'b')

exd0 = data.frame(x = x, y = y, z = z, stringsAsFactors = FALSE)
exd0 = exd0[order(y), ]

exd0

t0 = table(exd0$x)
p0 = prop.table(t0)
g0 = p0[1] * p0[2]

p0
g0

fit = rpart(x ~ y + z, data = exd, minsplit = 1, maxdepth = 4, cp = -1, minbucket = 1)
fit

# par(xpd = TRUE)

plot(fit, margin = 0.01, uniform = T)
text(fit, use.n = TRUE, xpd = T, minlength = 6)









exd0

exd1 = exd0[exd0$y >= 29, ]

t1 = table(exd1$x)
p1 = prop.table(t1)
g1 = p1[1] * p1[2]

t1
p1
g1
f2(p1[1], p1[2])




exd2 = exd1[exd1$z == 'b', ]
exd2

t2 = table(exd2$x)
p2 = prop.table(t2)
g2 = p2[1] * p2[2]

t2
p2
g2
f1(p2[1])



exd3 = exd1[exd1$z != 'b', ]
exd3

t3 = table(exd3$x)
p3 = prop.table(t3)
g3 = p3[1] * p3[2]

t3
p3
g3
f2(p3[1], p3[2])



fit

exd4 = exd0[exd0$y < 29, ]

t4 = table(exd4$x)
p4 = prop.table(t4)
g4 = p4[1] * p4[2]

t4
p4
g4
f2(p4[1], p4[2])




exd4
fit

exd5 = exd4[exd4$y < 25, ]

t5 = table(exd5$x)
p5 = prop.table(t5)
g5 = p5[1] * p5[2]

t5
p5
g5
f2(p5[1], p5[2])


exd5

exd6 = exd5[exd5$z == 'a', ]

t6 = table(exd6$x)
p6 = prop.table(t6)
g6 = p6[1] * p6[2]

t6
p6
g6
f2(p6[1], p6[2])


exd7 = exd5[exd5$z != 'a', ]

t7 = table(exd7$x)
p7 = prop.table(t7)
g7 = p7[1] * p7[2]

t7
p7
g7





