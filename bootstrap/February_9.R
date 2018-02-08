library(resampledata)
TV
library(dplyr)
t1 <- filter(TV, Cable == "Basic") %>% pull(Times)
t2 <- filter(TV, Cable == "Extended") %>% pull(Times)
diff_means <- mean(t1) - mean(t2)
bootstrap_sample_dist <- replicate(10000, {
  t1 <- t1[sample(10, 10, TRUE)]
  t2 <- t2[sample(10, 10, TRUE)]
  mean(t1) - mean(t2)
})
hist(bootstrap_sample_dist)
quantile(bootstrap_sample_dist, c(.025, .975)) #95% confidence interval 
t.test(TV$Time~TV$Cable) 

#We can also do a permutation test that the means are different:
permutation_means <- replicate(10000, {
  TV$Cable <- TV$Cable[sample(20)]
  mean(TV$Times[TV$Cable == "Basic"]) - mean(TV$Times[TV$Cable == "Extended"])
})
hist(permutation_means)
mean(permutation_means > diff_means)

#Compare this to the smallest bootstrap confidence interval that contains 0:
mean(bootstrap_sample_dist < 0)

#' This brings up a question: is the bootstrap confidence interval too narrow? Specifically,
#' if we take two samples from normal rvs and compute the bootstrap ci, will the true difference
#' of means be in there 95% of the time?

bb <- replicate(1000, {
  t1 <- rnorm(10, 1, 1)
  t2 <- rnorm(10, 0, 1) #true diff of means is 1
  bootstrap_sample_dist_2 <- replicate(1000, {
    t1 <- t1[sample(10, 10, TRUE)]
    t2 <- t2[sample(10, 10, TRUE)]
    mean(t1) - mean(t2)
  })
  1 < quantile(bootstrap_sample_dist_2, .975)  && 1 > quantile(bootstrap_sample_dist_2, .025)
})

#' When I ran this, I got 928 out of 1000 times the bootstrap CI did not contain the true 
#' difference of means. Does that mean it is not a 95% CI in the sense we are used to?

prop.test(928, 1000, .95)

#' Yes. With p = .001811 we can reject the hypothesis that the true percentage of times
#' that the 95% bootstrap CI contains the true difference of means is 0.95. 
#' 
#' Returning to the example, we can also estimate the bias and standard error as follows:

se <- sd(bootstrap_sample_dist) #Estimate for standard error of the statistic
mean(bootstrap_sample_dist) - diff_means #Estimate for the bias of the statistic

#' Example of biased estimator
t3 <- rexp(21, 1)
quantile(t3, .5) 
#' biased estimator of the mean; the median of an exponential with mean 1 is ln(2)
#' So, the bias is E[\hat \theta - \theta] = -0.30685

bootstrap_sample_dist <- replicate(10000, {
  t3_boot <- t3[sample(21, 21, TRUE)]
  quantile(t3_boot, .5)
})
mean(bootstrap_sample_dist) - mean(t3) #Compare to
log(2) - 1
