arsenic <- MnGroundwater$Arsenic
N <- length(arsenic)

bootstrap_sample_dist <- replicate(20000, {
  new_sample <- arsenic[sample(N, N, TRUE)]
  mean(new_sample)
})

hist(bootstrap_sample_dist)
ci <- quantile(bootstrap_sample_dist, c(.025, .5, .975))
ci[c(1,3)]
abline(v = ci, col = "red", lty = 2)
t.test(arsenic)
