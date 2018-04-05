#' To get confidence intervals for parameters in linear models, use confint.

x <- runif(30, 0, 10)
y <- 2 + 3 * x + rnorm(30, 0, 2)
mod <- lm(y~x)
str(mod)
summary(mod)
confint(mod)[1,2]
str(confint(mod, level = 0.99))

#' Let's verify that confint works correctly for the intercept via simulation.

sim_dat <- replicate(1000, {
  x <- runif(30, 0, 10)
  y <- 2 + 3 * x + rnorm(30, 0, 2)
  mod <- lm(y~x)
  2 > confint(mod)[1,1] && 2 < confint(mod)[1,2]
})
mean(sim_dat) #should be about 0.95

#' to find confidence and prediction intervals, use the R command predict

predict(mod, newdata = data.frame(x = 5), interval = "conf")
predict(mod, newdata = data.frame(x = 5), interval = "pre")

dat <- data.frame(x = x, y = y)
library(ggplot2)
ggplot(dat, aes(x , y)) + geom_point() + 
  geom_smooth(method = "lm")

conf_dat <- predict(mod, newdata = data.frame(x = seq(0,10,.1)), interval = "conf")
conf_dat <- data.frame(conf_dat, x = seq(0, 10, .1))
conf_dat <- tidyr::gather(conf_dat, key = bound, value = value, -x)
ggplot(dat, aes(x , y)) + geom_point() + 
  geom_smooth(method = "lm") +
  geom_line(data = conf_dat, mapping = aes(x = x,y = value, group = bound)) 


pre_dat <- predict(mod, newdata = data.frame(x = seq(0,10,.1)), interval = "pre")
pre_dat <- data.frame(pre_dat, x = seq(0, 10, .1))
pre_dat <- tidyr::gather(pre_dat, key = bound, value = value, -x)
ggplot(dat, aes(x , y)) + geom_point() + 
  geom_smooth(method = "lm") +
  geom_line(data = conf_dat, mapping = aes(x = x,y = value, group = bound)) +
  geom_line(data = pre_dat, mapping = aes(x = x,y = value, group = bound, style = "dashed"))
