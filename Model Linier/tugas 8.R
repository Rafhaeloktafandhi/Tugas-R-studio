install.packages("olsrr")
library(olsrr)
model_A <- lm(y ~ x1 + x_binary +  x1*x_binary, data = data1)
residi1 <- residuals(model_A)
residi2 <- rstandard(model_A)
hist(residi2, xlab = "residual", main = "")
ols_plot_resid_qq(model_A)

shapiro.test(residi1)
ks.test(residi2, "pnorm", mean = 0, sd = 1)
ols_test_normality(model_A)

library(lmtest)
ols_plot_resid_fit(model_A)
plot(fitted(model_A), rstandard(model_A))
bptest(model_A)


library(car)
durbinWatsonTest(model_A)

