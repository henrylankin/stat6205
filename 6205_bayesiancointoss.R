# simulate coin toss using bayesian statistics
# set.seed(1000)

# number of flips
N = 1000

# sample heads(1) and tails(0) values
flipsequence <- sample(x=c(0,1), prob = c(2/3,1/3), size = N, replace = TRUE)

# cumumlative distribution of the flipsequence
r <- cumsum(flipsequence)

# transform r into a cumulative probabilty distribution by dividing it all by the sum
n <- 1:N
runprop <- r/n

# plot convergence results
plot(n, runprop, type = 'o', log = 'x', xlim = c(1,N), ylim = c(0.0,1.0))
lines(c(1,N), c(0.5,0.5), lty = 3)
text(N, 0.3, paste("End proportion = ", runprop[N]), adj = c(1,0), cex = 1.5)

### grid search method with uniform prior
# Suppose we start with a prior belief that the probability of getting a head lies between (0.4, 0.7)

# data
data <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

# number of observatins
n <- length(data)

# create distribution of the prior ptheta
theta <- seq(from = 0.4, to = 0.7, by = 0.01)
ptheta.orig <- dunif(theta, 0, 1)
ptheta <- ptheta.orig/sum(ptheta.orig)

# number of heads & tails
nheads <- sum(data == 1)
ntails <- sum(data == 0)

# coin toss is a bernouli experiment, datagiventheta (the likelihood) is then a bernouli distribution
pdatagiventheta <- (theta^nheads)*(1-theta)^(ntails)

# model data probability distribution
pdata <- sum(pdatagiventheta*ptheta)

# use bayes rule to find pthetagiven data
pthetagivendata <- pdatagiventheta*ptheta/pdata

# plot prior, likelihood and posterior
par(mfrow = c(3,1))
plot(theta, ptheta, type = 'l', lwd = 3, main = 'prior')
plot(theta, pdatagiventheta, type = 'l', lwd = 3, main = 'likelihood')
plot(theta, pthetagivendata, type = 'l', lwd = 3, main = 'posterior')

### with beta prior using beta-binomial conjugate model (means we already have formula of posterior)

# create beta distribution of the prior, theta
binwidth = 0.005
theta = seq(from = binwidth/2, to = 1- (binwidth/2), by = binwidth)
a = 5
b = 9
ptheta = dbeta(theta, a, b)

# observed data
data=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

# number of observations, number of heads & tails
N = length(data)
nheads = sum(data == 1)
ntails = sum(data == 0) # = N-nheads

# calculate likelihood, a bernouli distribution given the prior ptheta
pdatagiventheta = (theta^nheads)*(1-theta)^ntails

# calculate posterior, by conjugate model we use a beta distribution with alpha=a+nheads, beta=N+b-nheads
pthetagivendata = dbeta(theta, a + nheads, N + b - nheads)


# plot prior, likelihood, posterior
par(mfrow = c(3,1))
plot(theta, ptheta, type = 'l', lwd = 3, main = "prior")
plot(theta, pdatagiventheta, type = 'l', lwd = 3, main = "likelihood")
plot(theta, pthetagivendata, type = 'l', lwd = 3, main = "posterior")



