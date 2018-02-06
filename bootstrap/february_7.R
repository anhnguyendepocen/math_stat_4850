library(resampledata)

wts <- NCBirths2004$Weight
bootstrap_sample_dist <- replicate(20000, {
  wts2 <- wts[sample(length(wts), length(wts), TRUE)]
  mean(wts2)
})
hist(bootstrap_sample_dist)

L <- quantile(bootstrap_sample_dist, .025) #Lower 95% CI bound
U <- quantile(bootstrap_sample_dist, .975) #Upper 95% CI bound
M <- mean(bootstrap_sample_dist)

abline(v = c(L, U, M), col = "red", lty = 2)

