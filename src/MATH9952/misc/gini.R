
# gini index

# install.packages("rgl")
library(rgl)
library(ggplot2)


gini.simple  = function(p) { 1 - p^2 - (1 - p)^2 }
gini.complex = function(p1, p2) { 1 - p1^2 - p2^2 - (1 - p1 - p2)^2 }
info.simple = function(p) { -(p * log(p) + (1 - p) * log(1 - p)) } 

x   = seq(0, 1, len = 400)
y1  = gini.simple(x)
y2  = info.simple(x)

plot(x, y2, type = 'l', lwd = 3, col = 'red', xlab = 'p', ylab = 'impurity')
lines(x, y1, lwd = 3, col = 'green')

# ggplot(data.frame(x, y1, y2), aes(x)) + 
#   geom_line(aes(y = y1), colour="red") + 
#   geom_line(aes(y = y2), colour="green")

# curve(gini.simple, from = 0, to = 1, lwd = 3, col = 'red', xlab = 'p', ylab = 'gini')


p1  = seq(0, 1, len = 40)
p2  = p1
z   = outer(p1, p2, Vectorize(gini.complex))


persp3d(p1, p2, z, zlab = 'gini', col = 'blue', lit = F, back = 'lines', front = 'lines')
spheres3d(0.3333, 0.3333, f2(0.3333, 0.3333), col = 'red', radius = 0.03)
