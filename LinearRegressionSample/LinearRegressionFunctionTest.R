install.packages("devtools")
library(devtools)
install_github(repo = "dungkhuc/PersonalRLib/LinearRegressionSample")
x = cbind(Const=1, Bwt=cats$Bwt)
y = cats$Hw
mod1 <- linmod(x, y)
mod1
summary(linmod(Hwt~Bwt+Sex, data=cats))
