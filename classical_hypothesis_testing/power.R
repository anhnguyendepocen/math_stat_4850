pt(q = qt(.99, 19), df = 19, ncp = 2, lower.tail = FALSE) +
  pt(q = -1 * qt(.99, 19), df = 19, ncp = 2, lower.tail = TRUE)

qt(.99, 19)

#' Let's test this with a simulation!
#' we take a random sample of size 20 from iid normal with mean 1 + 2 sd /sqrt(n) and sd 10.
#' Form confidence interval and see whether it is rejected

sig <- 10
n <- 20
dat <- rnorm(n = 20,1 + 2 * sig/sqrt(n)   ,sd = sig)
abs(t.test(dat, conf.level = 0.98,mu = 1)$statistic) > 2.539

pow_dat <- replicate(10000, {
  dat <- rnorm(n = 20,1 + 2 * sig/sqrt(n)   ,sd = sig)
  abs(t.test(dat, conf.level = 0.98, mu = 1)$statistic) > 2.539
})
mean(pow_dat) #compare to 0.3201187 Yay, us! we did it!
