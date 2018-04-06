#' To get confidence intervals for parameters in linear models, use confint.

x <- runif(30, 0, 10)
y <- 2 + 3 * x + rnorm(30, 0, 2)
plot(x, y)
mod <- lm(y~x)
str(mod)
summary(mod)
confint(mod, level = .99)
confint(mod)[1,2]
str(confint(mod, level = 0.99))
confint(mod)[2,]
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

sim_dat <- replicate(1000, {
  new_point <- 2 + 3 * 5 + rnorm(1, 0, 2)
  12.57995 < new_point && new_point < 22.23512
})
mean(sim_dat)



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





num_sample <- 300
dat_dat <- sapply(1:500, function(x) {
x <- runif(num_sample, 0, 10)
y <- 1 + 3 * x + rnorm(num_sample, 0, 4)
mod <- lm(y~x)
pre <- predict(mod, newdata = data.frame(x = 2), interval = "pre")
sim_dat <- replicate(300, {
  new_dat <- 1 + 3 * 2 + rnorm(1, 0, 4)
  new_dat > pre[2] && new_dat < pre[3]
})
mean(sim_dat)
})
mean(dat_dat < .9)
hist(dat_dat)

pre
sim_dat <- replicate(1000, {
  x <- runif(30, 0, 1)
  y <- 2 + 3 * x + rnorm(30, 0, 2)
  mod <- lm(y~x)
  pre <- predict(mod, newdata = data.frame(x = 5), 
                 interval = "pre")
  new_dat <- 2 + 3* 5 + rnorm(1, 0, 2)
  new_dat > pre[2] && new_dat < pre[3]
})
mean(sim_dat)
