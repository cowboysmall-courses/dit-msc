setwd('~/Workspace/College/DIT/MATH9952/Data')

data = read.csv('Advertise.csv', header = TRUE)

data

x_1 = data[, 1]
x_2 = x^2
x_3 = x^3
y   = data[, 2]


plot(x_1, y, pch = 20, cex = 1, col = 'orange')

fit_linear = lm(y ~ x_1)

lines(x, fit_linear$fitted, col = 'red', lwd = 2)

fit_quadratic = lm(y ~ x_1 + x_2)

s_x = seq(min(x_1), max(x_1), len = 200)
s_y = fit_quadratic$coeff[1] + fit_quadratic$coeff[2]*s_x + fit_quadratic$coeff[3]*s_x^2

lines(s_x, s_y, col = 'blue', lwd = 2)


fit_quadratic = lm(y ~ x_1 + x_2 + x_3)

s_x = seq(min(x_1), max(x_1), len = 200)
s_y = fit_quadratic$coeff[1] + fit_quadratic$coeff[2]*s_x + fit_quadratic$coeff[3]*s_x^2 + fit_quadratic$coeff[4]*s_x^3


lines(s_x, s_y, col = 'green', lwd = 2)
