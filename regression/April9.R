#' Bootstrap resampling from data

x <- runif(30, 0, 10)
y <- 1 + 2 * x + rnorm(30,0,3)
mod <- lm(y~x)
summary(mod)
predict(mod, newdata = data.frame(x = 5), interval = "pre")

boot_lower <- lapply(1:5000, function(t) {
  s <- sample(30, replace = TRUE)
  new_x <- x[s]
  new_y <- y[s]
  new_mod <- lm(new_y~new_x)
  as.numeric(predict(new_mod, newdata = data.frame(new_x = 5), interval = "pre")[2:3])
})
boots <- matrix(unlist(boot_lower), ncol = 2, byrow = TRUE)
#' Mean of bounds from bootstrap
apply(boots, 2, mean)
#' Parametric from formulas
predict(mod, newdata = data.frame(x = 5), interval = "pre")
#' MLE for bounds from bootstrap
sapply(1:2, function(t) density(boots[,t])$x[which.max(density(boots[,t])$y)])
#' Actual answer
11 + qnorm(c(.025, .975), 0, 3)

hist(boots)











#' Let's see the sampling distribution of the true probability of correctly predicting
#' for the bootstrap approach using mean, mle and the very conservative 90% CI

sim_data <- numeric(0)
sim_data <- c(sim_data, replicate(500, {
  x <- runif(30, 0, 10)
  y <- 1 + 2 * x + rnorm(30, 0, 3)
  #Other bootstrap
  mod <- lm(y~x)
  summary(mod)
  predict(mod, newdata = data.frame(x = 5), interval = "pre")
  
  boot_lower <- lapply(1:1000, function(t) {
    s <- sample(30, replace = TRUE)
    new_x <- x[s]
    new_y <- y[s]
    new_mod <- lm(new_y~new_x)
    as.numeric(predict(new_mod, newdata = data.frame(new_x = 5), interval = "pre")[2:3])
  })
  boots <- matrix(unlist(boot_lower), ncol = 2, byrow = TRUE)
  #' Mean of bounds from bootstrap
  as.vector(c(apply(boots, 2, mean), 
  #' MLE for bounds from bootstrap
  sapply(1:2, function(t) density(boots[,t])$x[which.max(density(boots[,t])$y)]),
  c(quantile(boots[,1], .05), quantile(boots[,2], .95)),
  as.vector(predict(new_mod, newdata = data.frame(new_x = 5), interval = "pre")[2:3])))
}))

mat <- matrix(sim_data, ncol = 8, byrow = TRUE)
pmat <- pnorm(mat, 11, 3)
hist(pmat[,2] - pmat[,1], probability = TRUE)
hist(pmat[,4] - pmat[,3], probability = TRUE)
hist(pmat[,6]  - pmat[,5])

#' Compare those histograms to what we get with predict

sim_predict <- replicate(10000, {
  x <- runif(30, 0, 10)
  y <- 1 + 2 * x + rnorm(30, 0, 3)
  mod <- lm(y~x)
  diff(pnorm(as.vector(predict(mod, newdata = data.frame(x = 5), interval = "pre")[2:3]), 11, 3))
})
hist(sim_predict, probability = TRUE)


dat <- data.frame(mean = pmat[,2] - pmat[,1],
                  mle = pmat[,4] - pmat[,3],
                  conservative = pmat[,6]  - pmat[,5],
                  parametric = sim_predict)
datplot <- tidyr::gather(dat, key = method, value = probability)
ggplot(datplot, aes(x = probability)) + geom_histogram(bins = 20) +
  facet_wrap(~method)

# save(list = c("datplot"), file = "april_9_datplot")
load("april_9")
datplot 
load("april_9_datplot")
group_by(datplot, method) %>% 
  summarize(n = mean(probability < 0.95))

