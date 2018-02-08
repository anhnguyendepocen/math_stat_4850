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
  bootstrap_sample_dist <- replicate(1000, {
    t1 <- t1[sample(10, 10, TRUE)]
    t2 <- t2[sample(10, 10, TRUE)]
    mean(t1) - mean(t2)
  })
  1 < quantile(bootstrap_sample_dist, .975)  && 1 > quantile(bootstrap_sample_dist, .025)
})

#' When I ran this, I got 928 out of 1000 times the bootstrap CI did not contain the true 
#' difference of means. Does that mean it is not a 95% CI in the sense we are used to?

prop.test(928, 1000, .95)

#' Yes. With p = .001811 we can reject the hypothesis that the true percentage of times
#' that the 95% bootstrap CI contains the true difference of means is 0.95. 