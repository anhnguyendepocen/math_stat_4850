set.seed(1)
library(resampledata)
head(NCBirths2004)

wts <- NCBirths2004$Weight
hist(wts, probability = TRUE) #This is a reasonable estimate of the distribution of weights of births in north carolina.

mu_hat <- mean(wts)
sigma_hat <- sd(wts)

mu_hat
sigma_hat
sigma_hat/sqrt(1009)
three_data_points <- wts[sample(1009, 3)]
three_data_points <- sort(three_data_points)

possible_samples <- sapply(1:27, function(x) three_data_points[c(ceiling(x/9), ((ceiling(x/3) - 1) %% 3) + 1 ,1 + ((x - 1) %% 3))])
bootstrap_samples <- t(possible_samples)
apply(bootstrap_samples, 1, mean) #Possible means
hist(apply(bootstrap_samples, 1, mean))

#' Sneakier way to do it
#' 
library(combinat)

p_s <- t(sapply(1:27, function(x) three_data_points[hcube(c(3,3,3))[x,]]))
hist(apply(p_s, 1, mean))


#'Now, we are going to do a real bootstrap sample

boot_sample <- sample(length(wts), replace = TRUE)
wts[boot_sample]
mean(wts[boot_sample])

boot_sample_dist <- replicate(10000, {
  boot_sample <- sample(length(wts), replace = TRUE)
  mean(wts[boot_sample])
})

#' this is the historgram of our bootstrap sampling distribution.
hist(boot_sample_dist)

sd(boot_sample_dist)
mean(boot_sample_dist) #' note that this is close to the original mean of the sample
