x <- rnorm(50, 1, 1)
y <-  x + rnorm(50, 1, 1)
cor(x, y)
1/sqrt(2)

x <- rexp(50)
y <- x + rexp(50)
plot(x, y)
my_dat <- data.frame(x = x, y = y)
sim_dat <- replicate(10000, {
  temp_dat <- my_dat[sample(1:50, replace = TRUE),]  
  cor(temp_dat$x, temp_dat$y)
})
hist(sim_dat)
quantile(sim_dat, c(0.025, 0.975))
cor.test(x, y)

met1 <- 0
met2 <- 0
for(i in 1:100) {
  x <- rnorm(50, 1, 1)
  y <- x + rnorm(50, 1, 1)
  my_dat <- data.frame(x = x, y = y)
  sim_dat <- replicate(1000, {
    temp_dat <- my_dat[sample(1:50, replace = TRUE),]  
    cor(temp_dat$x, temp_dat$y)
  })
  met1 <- met1 + ifelse(quantile(sim_dat, c(0.025)) < 1/sqrt(2) && 
                          quantile(sim_dat, c(0.975)) > 1/sqrt(2), 1, 0) 
  met2 <- met2 + ifelse(cor.test(x, y)$conf.int[1] < 1/sqrt(2) && 
                          cor.test(x, y)$conf.int[2] > 1/sqrt(2), 1, 0)
}
prop.test(x = 283, n = 300, p = 0.95)
met1


met1 <- 0
met2 <- 0
for(i in 1:100) {
  x <- rexp(50)
  y <- x + rexp(50)
  my_dat <- data.frame(x = x, y = y)
  sim_dat <- replicate(1000, {
    temp_dat <- my_dat[sample(1:50, replace = TRUE),]  
    cor(temp_dat$x, temp_dat$y)
  })
  met1 <- met1 + ifelse(quantile(sim_dat, c(0.025)) < 1/sqrt(2) && quantile(sim_dat, c(0.975)) > 1/sqrt(2), 1, 0) 
  met2 <- met2 + ifelse(cor.test(x, y)$conf.int[1] < 1/sqrt(2) && cor.test(x, y)$conf.int[2] > 1/sqrt(2), 1, 0)
}
prop.test(x = 182, n = 200, p = 0.95)
.9 * 4
