### Ex. 4
set.seed(1)

# generate a population of 5000 from exponential distribution with theta = 5
n <- 5000
theta <- 5
population <- rexp(n, rate = 1/theta)

# graph the population distribution
pop.hist <- hist(population, plot = FALSE)
pop.hist$counts <- pop.hist$counts/sum(pop.hist$counts)
plot(pop.hist, col = 'light green', main = 'Sample from Exponential Distribution', 
     xlab = 'sampled values', ylab = 'density')

mean.pop <- mean(population)
sd.pop <- sd(population)

# print mean and sd of population
print(sprintf("mean of sampled expopnetial values: %f, variance of sampled exponential values: %f", mean.pop, sd.pop^2))

# create blank vector
mean.sample <- c()

# take 1000 samples of size 40 from the populaiton and find their sample mean, add sample mean to mean.sample
for (i in 1:1000) {
  xbar.sample <- sample(population, 40)
  mean.sample <- c(mean.sample, mean(xbar.sample))
}

# graph the sample mean distribution, calculate mean and sd of sample means
xbar.hist <- hist(mean.sample, plot = FALSE)
xbar.hist$counts <- xbar.hist$counts/sum(xbar.hist$counts)
plot(xbar.hist, col = 'light blue', main = 'Sample Mean Distribution', 
     xlab = 'sample mean values', ylab = 'density')

mean.xbar <- mean(mean.sample)
sd.xbar <- sd(mean.sample)

# print mean and sd of samples
print(sprintf("mean of sample means: %f, variance of sample means: %f", mean.xbar, sd.xbar^2))