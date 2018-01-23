#Create data frame of the type needed for permutation test

bully_data <- matrix(c(rep(c("S", "B"), 42),
         rep(c("S", "N"), 50),
         rep(c("N", "B"), 30), 
         rep(c("N", "N"), 87)), ncol = 2, byrow = TRUE)
bully_data <- as.data.frame(bully_data)

str(bully_data)
names(bully_data) <- c("Height", "Bullied")
table(bully_data) #This is in a different order than we had it in our table!

bully_data$Height <- factor(bully_data$Height, levels = c("S", "N"))
table(bully_data)  #Now it is in the correct order. Just so I don't get confused again!!
tabled_data <- table(bully_data) 

rows <- apply(X = tabled_data, MARGIN = 1, sum)
cols <- apply(tabled_data, 2, sum)
expected_table <- rows %*% t(cols)/sum(tabled_data)  #This is an automated way to compute the expected contingency table!


test_stat <- sum((tabled_data - expected_table)^2/expected_table)

#' Now we want to permute the second column a bunch of times, while repeating the computation above
#' for the permuted data frame. First, we do a permutation and repeat the above computation, then we'll
#' put it inside replicate. 

bully_data$Bullied <- sample(bully_data$Bullied)
tabled_data <- table(bully_data) 
sum((tabled_data - expected_table)^2/expected_table)

#' That was pretty easy. Note that when we repeat, we take a random permutation of a permutation, which
#' is just the same (probabilistically) as taking a random permutation.

null_dist <- replicate(5000, {
  bully_data$Bullied <- sample(bully_data$Bullied)
  tabled_data <- table(bully_data) 
  sum((tabled_data - expected_table)^2/expected_table)
})

#' Let's look at a histogram.

hist(null_dist)
abline(v = test_stat, col = "blue", lty = 2)

#' This is pretty strong evidence against the null hypotesis! Let's check out the $p$-value.

mean(null_dist >= test_stat)

#' If we are being really careful here, then we need to add 1 to the upstairs and downstairs
#' of the previous computation. That doesn't change much, but it does keep the $p$-value from being
#' (estimated at) exactly zero, which would be impossible.

(sum(null_dist >= test_stat) + 1)/(5001)

#' This really doesn't change much.
#' 
#' 
#' Another Example, and relation to chi-squared distributions
#' 

candy_data <- matrix(c(rep(c("B", 1), 42),
                       rep(c("B", 2), 20),
                       rep(c("B", 3), 38),
                       rep(c("G", 1), 33),
                       rep(c("G", 2), 27),
                       rep(c("G", 3), 50)
                       ), byrow = TRUE, ncol = 2)
candy_data
candy_data <- data.frame(candy_data)

names(candy_data) <- c("Gender", "Candy")
table(candy_data)

tabled_candy <- table(candy_data)

row <- apply(tabled_candy, 1, sum)
col <- apply(tabled_candy, 2, sum)

expected_candy <- row %*% t(col)/sum(tabled_candy)
expected_candy
test_stat <- sum((expected_candy - tabled_candy)^2/expected_candy)

null_dist <- replicate(5000, {
  candy_data$Candy <- sample(candy_data$Candy)
  tabled_candy <- table(candy_data)
  sum((expected_candy - tabled_candy)^2/expected_candy)
})

hist(null_dist, probability = TRUE)
abline(v = test_stat, col = "green", lty = 2)
curve(expr = dchisq(x, 2), from = 0, to = 17, add = TRUE, col = "blue", lty = 3)

#' Another example
#' 
bdays <- c(150, 138, 140, 100)
expected_bdays <- rep(sum(bdays)/4, 4)
t_stat <- sum((expected_bdays - bdays)^2/expected_bdays)
null_dist <- replicate(5000, 
  sum((rmultinom(1, sum(bdays), c(1/4,1/4,1/4,1/4)) - expected_bdays)^2/expected_bdays)
)

hist(null_dist, probability = TRUE)
curve(dchisq(x, 3), from = 0, to = 20, add = TRUE, col = "blue", lty = 2)
