n <- 30
x <- 1:n/3
y <- 1 + 2 * x + rnorm(n, 0, 2)
mod <- lm(y~x)
sig <- sqrt(sum(mod$residuals^2)/(n-2)) #OR summary(mod)$sigma
sxx <- sqrt(sum((x - mean(x))^2))
beta_1 <- lm(y~x)$coefficients[2]
2 < beta_1 + qt(.975, n - 2) * sig/sxx & beta_1 - qt(.975, n-2) * sig/sxx

sim_dat <- replicate(5000, {
  y <- 1 + 2 * x + rnorm(n, 0, 2)
  mod <- lm(y~x)
  sig <- sqrt(sum(mod$residuals^2)/(n-2)) #OR summary(mod)$sigma
  sxx <- sqrt(sum((x - mean(x))^2))
  beta_1 <- lm(y~x)$coefficients[2]
  2 < beta_1 + qt(.975, n - 2) * sig/sxx & 2 >  beta_1 - qt(.975, n-2) * sig/sxx
})
mean(sim_dat)
summary(lm(y~x))
sig/sxx 
#' Note this is the Std. Error of the coefficient. We can compute confidence
#' intervals of the intercept if we trust R's computation of the std error, and
#' we can correctly determine the number of degrees of freedom!
#' 
#' Or, we can use the built in R function confint, as so:

confint(mod, level = 0.99)
beta_0 <- summary(mod)$coefficients[1]
beta_0 + qt(.995, 28) * summary(mod)$coefficients[1,2] #Note that they are the same. YAY!

