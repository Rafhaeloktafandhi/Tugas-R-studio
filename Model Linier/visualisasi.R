library(ggplot2)

ggplot(data1, aes(x1, y, color=factor(x_binary))) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(color="Gender (0=F,1=M)", title="Fitted line Total_Score vs Stress_Level")

# 2. Plot residuals vs fitted
plot(model$fitted.values, resid(model),
     xlab="Fitted values", ylab="Residuals",
     main="Residuals vs Fitted")
abline(h=0, col="red", lty=2)

# 3. Q-Q plot normalitas residual
qqnorm(resid(model), main="Q-Q Plot of Residuals")
qqline(resid(model), col="red")


data2$Family_Income_Level <- factor(data2$Family_Income_Level)  # pastikan bertipe faktor
x3<- model.matrix(~ Family_Income_Level, data = data2)
model.matrix(y ~ Family_Income_Level, data = data2)
x4<- factor(data2$Family_Income_Level, levels = c("High", "Medium", "Low"))
