# background
# https://stackoverflow.com/questions/38109501/how-does-predict-lm-compute-confidence-interval-and-prediction-interval

set.seed(42)
df <- data.frame(y=runif(10), 
                 x1=rnorm(10), 
                 x2 = sample(1:3, 10, replace = TRUE), 
                 x3 = sample(1:5, 10,replace = TRUE), 
                 x4 = sample(1:2, 10,replace = TRUE))
fit <- lm(y ~ x1+x2+x3+x4, df)




test <-  data.frame(
                   x1=rnorm(1), 
                   x2 = sample(1:3, 1, replace = TRUE), 
                   x3 = sample(1:5, 1,replace = TRUE), 
                   x4 = sample(1:2, 1,replace = TRUE))

predict.lm(fit, test, interval = "prediction", level = c(0.9))



z <- predict(fit, test, se.fit=TRUE)

se.PI <- sqrt(z$se.fit^2 + z$residual.scale^2)

level <- 0.9
  
alpha <- qt((1-level)/2, df = z$df)

PI <- z$fit + c(alpha, -alpha) * se.PI


