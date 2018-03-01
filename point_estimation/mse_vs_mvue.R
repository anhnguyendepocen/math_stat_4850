squared <- replicate(10000, {
  s1 <- rnorm(10, 2, 4)
  (sd(s1)^2 - 16)^2
})
mean(squared)

biased <- replicate(10000, {
  s1 <- rnorm(10, 2, 4)
  ((9/10*sd(s1))^2 - 16)^2
})
mean(biased)

unbiased <- replicate(10000, {
  sd(rnorm(10, 2, 4))^2
})
mean(unbiased)

mean(replicate(10000, {
  (.9 * sd(rnorm(10, 2, 4)))^2
}))
