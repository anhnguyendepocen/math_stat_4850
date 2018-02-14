#' Problems with the bootstrap associated with discrete empirical cdf

set.seed(213)
my_dat <- rnorm(11, 0, 10)
plot(ecdf(my_dat))
bootstrap_sample <- replicate(10000, {
  quantile(sample(my_dat, replace = TRUE), .5)
})
hist(bootstrap_sample, probability = TRUE)

#' compare to what an estimate of the sampling distribution of the median should look like; this is
#' terrible

median_dist <- replicate(10000, {
  quantile(rnorm(11, 0, 10), .5)
})
hist(median_dist, probability = TRUE)

#' The problem is the discrete nature of the empirical cdf. We need to smooth it to get better results.
#' This brings up a question; how do you smooth an empirical cdf? We take a different approach
#' and smooth the histogram associated with the bootstrap resample, and sample from that.

#' Let's investigate the role that the standard deviation in kernel density estimation plays.

plot(density(my_dat, bw = 1)) #too wiggly
plot(density(my_dat, bw = 50)) #just returns a normal rv with mean 0 and sd 50!
curve(dnorm(x, 0, 50), add = TRUE, col = "red")

plot(density(my_dat, bw = "sj")) 
#' seems to be a good compromise. smooths things out but not too much. there are also theoretical 
#' reasons to like this, at least asymptotically.

#' here, we sample from the kde, and check that the histogram matches the density.

sigma <- density(my_dat, bw = "sj")$bw

kde_sample <- replicate(10000, {
  rnorm(1, sample(my_dat, 1), sigma)
})
hist(kde_sample, probability = TRUE, add = TRUE) #Yes, seems to be sampling from the kde


#' Now, we can adjust our bootstrap resampling to have a smooth bootstrap.

bw <- density(my_dat, bw = "sj")$bw

eleven_data_points <- sample(my_dat, size = 11, replace = TRUE)
eleven_data_points
rnorm(11, eleven_data_points, bw)
quantile(rnorm(11, eleven_data_points, bw), 0.5)

kde_data <- replicate(10000, {
  eleven_data_points <- sample(my_dat, size = 11, replace = TRUE)
  eleven_data_points
  rnorm(11, eleven_data_points, bw)
  quantile(rnorm(11, eleven_data_points, bw), 0.5)
})
hist(kde_data + 5, probability = TRUE, breaks = seq(-30,30,2.5)  ) #Estimate for the sampling distribution of the median - note that it is biased

#' We actually know that this data is normal with sd 10, so we can get a much better estimate
#' of the sampling distribution that way.

better_est <- replicate(10000, {
  quantile(rnorm(11, 0, 10), 0.5)
})
hist(better_est, probability = TRUE, col = 'red', breaks = seq(-25,25,2.5))
hist(kde_data + 5, probability = TRUE, breaks = seq(-25,25,2.5), add = TRUE  ) #Estimate for the sampling distribution of the median - note that it is biased

#'This isn't the best, but remember, we only have 11 data points!!!! And we are trying to estimate
#'the sampling distribution of the median, from that, which should have seemed impossible before
#'starting this class!!!
