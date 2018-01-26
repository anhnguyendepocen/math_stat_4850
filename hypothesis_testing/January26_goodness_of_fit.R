#' Goodness of fit --- all parameters known

dat <- rnorm(50, 20, 20) #Let's see whether R's random number generator passes our goodness of fit test.

binned_dat <- cut(x = dat, breaks = qnorm(c(0,.2, .4, .6, .8, 1), 20, 20), labels = 1:5)
table(binned_dat) #This should be a random sample from a multinomial distribution with all probs = 1/5

expected_table <- rep(10, 5)
test_stat <- sum((expected_table - table(binned_dat))^2/expected_table)

null_dist <- replicate(10000, {
  sum((rmultinom(1,50,rep(.2,5)) - expected_table)^2/expected_table)
})

hist(null_dist, probability = TRUE)
curve(dchisq(x, 4), col = "blue", lty = 2, add = TRUE)
abline(v = test_stat, col = "red", lty = 3)
mean(test_stat < null_dist)


#' Goodness of fit - some parameters unknown!

library(resampledata)
hr_data <- table(Phillies2009$HomeRuns)

hr_data <- c(hr_data[1:4], 10)

hr_data #Is this Poisson? What would the rate be?
hr_rate <- mean(Phillies2009$HomeRuns) #this gives the mean rate of home runs

hr_probs <- c(dpois(0:3, hr_rate), 1 - sum(dpois(0:3, hr_rate)))
nrow(Phillies2009)

expected_hr <- hr_probs * 162
t_stat <- sum((expected_hr - hr_data)^2/expected_table)

#' DANGER!!!
null_dist <- replicate(5000, {
  sim_dat <- rmultinom(1, 162, hr_probs)
  sum((expected_hr - sim_dat)^2/expected_hr)
})

hist(null_dist, probability = TRUE)
curve(dchisq(x,4), from = 0, to = 10,col = "red", add = TRUE)
curve(dchisq(x,3), from = 0, to = 10,col = "blue", add = TRUE)

#' Why is red better, even though the theory tells us that blue should be better? Why have messed
#' up our simulation! We need to re-simulate, and to estimate the probabilities of the bins each time
#' using the estimated rate of the Poisson. Uh-oh.

hr_sim <- rpois(162, hr_rate)
hr_sim_rate <- mean(hr_sim)
hr_sim_bin <- table(hr_sim)

hr_sim_probs <- c(dpois(0:3, hr_sim_rate), 1 - sum(dpois(0:3, hr_sim_rate)))
hr_sim_expected <- hr_sim_probs * 162

null_dist <- replicate(5000, {
  hr_sim <- rpois(162, hr_rate)
  hr_sim_rate <- mean(hr_sim)
  hr_sim_bin <- table(hr_sim)
  hr_sim_bin <- c(hr_sim_bin[1:4], sum(hr_sim > 3))
  
  hr_sim_probs <- c(dpois(0:3, hr_sim_rate), 1 - sum(dpois(0:3, hr_sim_rate)))
  hr_sim_expected <- hr_sim_probs * 162
  sum((hr_sim_expected - hr_sim_bin)^2/hr_sim_expected)
})

hist(null_dist, probability = TRUE)  
curve(dchisq(x,4), from = 0, to = 10,col = "red", add = TRUE)
curve(dchisq(x,3), from = 0, to = 10,col = "blue", add = TRUE) #Now the blue one looks right! Yay!

mean(null_dist >= t_stat)
