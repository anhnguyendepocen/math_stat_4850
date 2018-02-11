dat <- rnorm(10)
old_dat <- dat
boot_data <- replicate(10000, {
  dat <- dat[sample(10, 10, TRUE)]
  m1 <- mean(dat)
  sqrt(1/10 * sum ((dat - m1)^2))
})
hist(boot_data)

m1 <- mean(old_dat)
mean(boot_data) - sqrt(1/10 * sum ((old_dat - m1)^2))

#' True bias:
sqrt(1/10 * sum ((old_dat - m1)^2)) - sd(old_dat)
