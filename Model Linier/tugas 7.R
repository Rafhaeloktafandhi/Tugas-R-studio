rm(list=ls())
data2 <- read.xlsx("Model Linier/hasil_preprocessing_modlin.xlsx")

rm(list=ls())
data1<-read.csv(file="D:/New folder/Students_Grading_Dataset.csv", sep = ";")

x_binary <- ifelse(data1$Gender == "Female", 0, 1)


x <- data2$Participation_Score
y <- data2$Total_Score
x4<- factor(data2$Family_Income_Level, levels = c("High", "Medium", "Low"))
fit_Interaksi <- lm(y ~ x4*x)
summary(fit_Interaksi)

fit_orde_2 <- lm(y ~ x + I(x**2))
summary(fit_orde_2)

model_A <- lm(y ~ x4 + x +  x4*x, data = data2)
anova(model_A)
model_B <- lm(y ~ x4 + x +  x4*x + I(x**2), data = data2)
anova(model_B)
anova(model_A, model_B)


