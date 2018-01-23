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