rm(list=ls())
data1<-read.csv(file="D:/New folder/Students_Grading_Dataset.csv", sep = ";")

x_binary <- ifelse(data1$Gender == "Female", 0, 1)

x <- data1$Gender
y <- data1$Total_Score
x1 <- data1$Stress_Level..1.10.
x2 <- 
fit_Interaksi <- lm(y ~ x1*x_binary)
summary(fit_Interaksi)

fit_orde_2 <- lm(y ~ x1 + I(x1**2))
summary(fit_orde_2)

model_A <- lm(y ~ x1 + x_binary +  x1*x_binary, data = data1)
anova(model_A)
model_B <- lm(y ~ x1 + x_binary +  x1*x_binary+ I(x1**2) + I(x_binary**2), data = data1)
anova(model_B)
anova(model_A, model_B)

