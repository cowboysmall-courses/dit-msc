
library(rpart)

churn     = rep(c("yes", "no"), c(5, 4))
age       = c(28, 28, 24, 24, 26, 28, 26, 26, 24)
allowance = c('A', 'D', 'A', 'D', 'D', 'A', 'D', 'D', 'A')

Churn     = data.frame(Churn = churn, Age = age, Allowance = allowance, stringsAsFactors = FALSE)
Churn

tfit      = rpart(Churn ~ Age + Allowance, data = Churn, minsplit = 1, maxdepth = 2, cp = -1, minbucket = 1)
tfit

plot(tfit, margin = 0.01, uniform = T)
text(tfit, use.n = T, xpd = T, minlength = 6)

Churn[Churn$Allowance == 'A', ]
Churn[Churn$Allowance == 'D', ]

Churn[Churn$Age >= 25, ]
Churn[Churn$Age <  25, ]

Churn[Churn$Age >= 27, ]
Churn[Churn$Age <  27, ]
