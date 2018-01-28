#' We estimate the sampling distribution of the proportion of heads obtained when 10 coins are tossed.

coin_toss <- sample(0:1, 10, TRUE) #0 is tails and 1 is heads
sum(coin_toss) #this is the number of heads observed
mean(coin_toss) #this is the proportion of heads observed

sampling_dist <- replicate(5000, {
  coin_toss <- sample(0:1, 10, TRUE) #0 is tails and 1 is heads
  mean(coin_toss) #this is the proportion of heads observed
})

hist(sampling_dist, probability = TRUE)

#' Estimate the sampling distribution for the sample mean when 100 samples are drawn from exponential
#' with rate 1/15.

exp_sample <- rexp(n = 100, rate = 1/15)
mean(exp_sample)

sampling_dist <- replicate(5000, {
  exp_sample <- rexp(n = 100, rate = 1/15)
  mean(exp_sample)
})

hist(sampling_dist, probability = TRUE)
qqnorm(sampling_dist)
qqline(sampling_dist) #we see that the sampling distribution is close to normal, but still somewhat skew.

#' In order to estimate the expected value and standard deviation of X bar, we can use

mean(sampling_dist)
sd(sampling_dist)

#We see that these are pretty good estimates of the mean and standard deviation of the sample mean.

#' Estimate the sample distribution of the maximum of 12 numbers drawn uniformly from 0 to 1.

uniform_sample <- runif(12,0,1)
max(uniform_sample)

sampling_dist <- replicate(5000, {
  uniform_sample <- runif(12,0,1)
  max(uniform_sample)
})

hist(sampling_dist, probability = TRUE)
curve(12 *x ^(11), from = 0, to = 1, add = TRUE, col = "blue") #check whether this matches the theory we developed


#' Compute the probability that the second largest of 10 random samples from exponential with 
#' rate 1/2 is greater than or equal to 4. 

1 - integrate(f = function(x) 180 * exp(-4 * x) * (1 - exp(-2*x))^8, lower = 0, upper = 2)$value

exp_sample <- rexp(10, rate = 2)
sort(exp_sample)[9]  #if we had more than 10, we might want to only do a partial sort

sampling_dist <- replicate(5000, {
  exp_sample <- rexp(10, rate = 2)
  sort(exp_sample)[9]  #if we had more than 10, we might want to only do a partial sort
})

hist(sampling_dist, probability = TRUE)
mean(sampling_dist > 2)


#' Consider the Phillies data from resampledata. Is the strikeout data poisson?
library(resampledata)
so <- Phillies2009$StrikeOuts
table(so)
my_dat <- c(12, 16, 16, 24, 22, 24, 25, 10, 13)
mean(so)
expected <- c(sum(dpois(0:3, 7.12963)), dpois(4:10, 7.12963), 1 - ppois(10, 7.12963))
test_stat <- sum((my_dat - expected * 162)^2/(expected * 162))
pchisq(test_stat, df = 7, lower.tail = FALSE)

#' Passes that test
#' Note that we have a maximum number of strikeouts of 20.

sampling_dist <- replicate(5000, {
  so_sample <- rpois(162, mean(so))
  max(so_sample)
})

hist(sampling_dist)
mean(sampling_dist >= 20)
#' Unlikely that we would have 20 or more strikeouts in one game if the distirbution really is Poisson. 


