rm(list=ls())
data1<-read.csv(file="Model Linier/Students_Grading_Dataset.csv", sep = ";")

x_binary <- ifelse(data1$Gender == "Female", 0, 1)

x <- data1$Gender
y <- data1$Total_Score
x1 <- data1$Stress_Level.(1-10)
x2 <- data1$Study_Hours_per_Week
fit_Interaksi <- lm(y ~ x1*x)
cov_xy <- cov(x1, y)
var_x <- var(x1)
var_y <- var(y)

# Menghitung koefisien regresi
b1 <- cov_xy / var_x
b0 <- mean(y) - b1 * mean(x1)

# Menampilkan hasil
cat("Cov(x, y):", cov_xy, "\n")
cat("Var(x):", var_x, "\n")
cat("Var(y):", var_y, "\n")
cat("Koefisien regresi b1 (slope):", b1, "\n")
cat("Koefisien regresi b0 (intercept):", b0, "\n")

