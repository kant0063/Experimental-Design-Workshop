#Bootstrap examples

library(bootstrap)
print(mouse.c) #mouse data
x <- mouse.c #make mouse data x
theta.hat <- mean(x) #get mean of empirical distribution
print(theta.hat) #get mean

nboot <- 1000 #set number of bootstrap replication
theta.star <- double(nboot) # create a place to place all bootstraps

#The for loop makes nboot bootstrap samples x.star and 
#calculates for each the statistical functional of interest theta.star[i], 
#which in this example is the mean

for (i in 1:nboot) {
  x.star <- sample(x, replace = TRUE) # function for library bootstrap
  theta.star[i] <- mean(x.star) #storing the sampled mean
}
hist(theta.star) #histrogram of mean
abline(v = theta.hat, lty = 2) # add bootstrap mean
sd(theta.star) #bootstrap standard deviation
# theoretical value for comparison
sqrt(mean((x - mean(x))^2) / length(x))

##bootstrap CI
conf.level <- 0.95 #set confidence level
nboot <- 1000 # set number of bootstrap replications

x <- mouse.t
theta.hat <- mean(x)

theta.star <- double(nboot)
for (i in 1:nboot) {
  x.star <- sample(x, replace = TRUE)
  theta.star[i] <- mean(x.star)
}
hist(theta.star)
abline(v = theta.hat, lty = 2)
probs <- (1 + c(-1, 1) * conf.level) / 2
quantile(theta.star, probs = probs)
abline(v = (quantile(theta.star, probs = probs)), col="red")
