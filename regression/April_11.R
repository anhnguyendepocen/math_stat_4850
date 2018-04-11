#' Bootstrap confidence intervals

x <- runif(30, 0, 10)
y <- 1 + 2*x + rnorm(30, 0, 3)\
plot_dat <- data.frame(x = x, y = y)

endpoints <- replicate(500, {
  s <- sample(30, replace = TRUE)
  new_x <- x[s]
  new_y <- y[s]
  as.numeric(predict(lm(new_y~ new_x), newdata = data.frame(new_x = c(0, 10))))
})
endpoints <- data.frame(t(endpoints))
names(coefficients) <- c("zero", "ten")


long_endpoints <- tidyr::gather(endpoints, key = x_value, value = y_value)
long_endpoints$group <- 1:500
long_endpoints$x_value <- c(rep(0,500), rep(10, 500))

library(ggplot2)
ggplot(plot_dat, mapping = aes(x, y)) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  geom_line(data = long_endpoints, aes(x = x_value,
                                       y = y_value,
                                       group = group),
            color = "red",
            alpha = 0.02)
  
library(resampledata)
Bushmeat2
plot(Bushmeat2)


m <- min(Bushmeat2$Fish)
M <- max(Bushmeat2$Fish)
endpoints <- replicate(500, {
  s <- sample(nrow(Bushmeat2), replace = TRUE)
  new_bush <- Bushmeat2[s,]
  as.numeric(predict(lm(Change ~ Fish, data = new_bush), newdata = data.frame(Fish = c(m, M))))
})
endpoints <- data.frame(t(endpoints))
names(coefficients) <- c("m", "M")


long_endpoints <- tidyr::gather(endpoints, key = x_value, value = y_value)
long_endpoints$group <- 1:500
long_endpoints$x_value <- c(rep(m,500), rep(M, 500))
ggplot(Bushmeat2, mapping = aes(Fish, Change)) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  geom_line(data = long_endpoints, aes(x = x_value,
                                       y = y_value,
                                       group = group),
            color = "red",
            alpha = 0.1)
