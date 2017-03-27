set.seed(1)

# original data
particle_count <- c(4, 1, 3, 1, 3, 3, 3, 1, 1, 1, 6, 4, 4, 2, 2, 1, 1, 4, 2, 5)

### part (a)

# calculate original mean
orig_mean <- mean(particle_count)
print(sprintf("Original mean: %f", orig_mean))

# length of each bootstrap sample n, number of bootstrap samples, sig level
n <- length(particle_count)
B <- 5000
alpha <- 0.05

# draw B samples of size n with replacement (really one large sample of size B*n)
bss <- sample(particle_count, B*n, replace = TRUE)

# organize sample into a matrix with nrow=B, bcol=n
bss_matrix <- matrix(bss, nrow = B)

# find mean of each sample (row of the matrix) --> this serves as the sampling distribution of the bootsrap sample means
bs_mean <- rowMeans(bss_matrix)

# confidence interval of true parameter from boostrap samples
bci <- quantile(bs_mean, c(alpha/2, 1 - alpha/2))
print(bci)

### part (b)

# posterior dist = gamma(a + sum(data), n + b)
# prior parameters
a <- 2
b <- 1
# posterior parameters
a.posterior <- a + sum(particle_count)
b.posterior <- n + b
theoretical.mean <- a.posterior/b.posterior

# sample from the posterior distribution, find simulated mean
posterior.sample <- rgamma(5000, a.posterior, b.posterior)
posterior.sample.mean <- mean(posterior.sample)
print(sprintf("posterior simulated mean = %f", posterior.sample.mean))

# create gamma distribution of the prior, lambda, using parameters a=2,b=1 so that mean=variance=2
#lambda = seq(from = 0.1, to = 5.0, by = 0.1)
#plambda <- dgamma(lambda, a, b)
lambda <- rgamma(50, a, b)
plambda <- density(lambda) # a=2, b=1
prior.mean <- mean(lambda)
prior.sd <- sd(lambda)
# number of observations = n

# calculate likelihood, a poisson distribution given the prior lambda
likelihood <- function(data, parameter) {
  likelihood <- 1
  for (i in 1:length(data)) {
    likelihood = likelihood*dpois(data[i], parameter)
  }
  return(likelihood)
}
pdatagivenlambda <- likelihood(particle_count, sort(lambda))

# calculate posterior, by conjugate model we use a beta distribution with alpha=sum of data, beta=n+1
#plambdagivendata <- dgamma(lambda, a.posterior, b.posterior)
#posterior.sample <- rgamma(5000, a.posterior, b.posterior)
plambdagivendata <- density(posterior.sample)

par(mfrow = c(1,1))
hist(posterior.sample, freq = FALSE)
hist.text <- sprintf("mean = %s", round(posterior.sample.mean, 3))
text(3.5, 1, hist.text)
#posterior.hist$counts <- posterior.hist$counts/sum(posterior.hist$counts)
#plot(posterior.hist, ylim = c(0, 5), ylab = 'probability', main = "Posterior Distribution")
lines(plambdagivendata, lwd = '3')

# plot prior, likelihood, posterior
par(mfrow = c(3,1))
plot(plambda, type = 'l', lwd = 3, main = "prior", xlab = 'lambda', 
     xlim = c(0.0,4.0), ylab = 'plambda', xaxt = 'n')
axis(side = 1, at = seq(0, 4.0, 0.5))
plot(sort(lambda), pdatagivenlambda, main = "likelihood", type = 'l', 
      lwd = 3, xlab = 'lambda', xlim = c(0.0,4.0), xaxt = 'n')
axis(side = 1, at = seq(0, 4.0, 0.5))
plot(plambdagivendata, type = 'l', lwd = 3, main = "posterior",  
     xlab = 'lambda', xlim = c(0, 4.0), ylab = 'plamdagivendata', xaxt = 'n')
axis(side = 1, at = seq(0, 4.0, 0.5))



