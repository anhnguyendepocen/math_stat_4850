x <- rnorm(50, 1, 1)
y <- x + rnorm(50, 1, 1)
plot(x, y)
cor(x, y)
1/sqrt(2)

cor.test(x, y)
my_dat <- data.frame(x = x, y = y)
my_dat
sampling_dist <- replicate(10000, {
  boot_sample <- my_dat[sample(1:50, 
                               replace = TRUE), ]  
  cor(boot_sample$x, boot_sample$y)
})
quantile(sampling_dist, c(.025, .975))
cor.test(x, y)
hist(sampling_dist)
