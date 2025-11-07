library(car)

model_A <- lm(y ~ x1+ x_binary + x1*x_binary, data = data1)
model_B <- lm(y ~ x1 + x_binary +  x1*x_binary+ I(x1**2) + I(x_binary**2), data = data1)
pt <- powerTransform(model_A, family = "bcPower")
lambda <- pt$lambda
data1$y_trans <- bcPower(y, 0.66)

model_trans <- lm(y_trans ~ x1 + x_binary + x1*x_binary, data = data1)
e2 <- rstandard(model_trans)
shapiro.test(e2) 
`summary(pt)
par(mfrow = c(2, 2))
plot(model_trans)                # termasuk Residuals vs Fitted
qqPlot(model_trans, simulate=TRUE)
qqPlot

model_poly <- lm(y_trans ~ poly(y, 2) + x_binary + poly(y, 2):x_binary, data = data1)
shapiro.test(rstandard(model_poly))

pt_yj <- powerTransform(y ~ x1 + x_binary + x1:x_binary, data = data1, family = "yjPower")
lambda_yj <- pt_yj$lambda
data1$y_yj <-yjPower(data1$total_score, lambda = lambda_yj[1])

model_yj <- lm(y_yj ~ x1 + x_binary + x1:x_binary, data = data1)
shapiro.test(rstandard(model_yj))

model_rob <- rlm(y ~ x1 + x_binary + x1:x_binary, data = data1)
summary(model_rob)

data1$y_yj <- yjPower(data1$total_score, lambda = lambda_yj[1])

lambda_numeric <- as.numeric(lambda_yj)
data1$y_yj <- yjPower(data1$total_score, lambda = lambda_numeric)

model_red <- lm(y ~ x1 + x_binary +  x1*x_binary+ I(x1*2) + I(x_binary*2))
summary(model_red)

shapiro.test(rstandard(model_red))
y_pred <- predict(model_A)
rmse <- sqrt(mean((data1$y - y_pred)^2))
print(rmse)
model_B <- lm(y ~ x1 + x_binary + x1*x_binary + I(x1^2) + I(x_binary^2), data = data1)

# Prediksi nilai y menggunakan model B
y_pred_B <- predict(model_B)

# Hitung RMSE untuk model B
rmse_B <- sqrt(mean((data1$y - y_pred_B)^2))
print(rmse_B)

