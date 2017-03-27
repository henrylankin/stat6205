### part (b)

# data
n <- 8197 
nPos <- 51
nNeg <- n - nPos
hivcount <- c(rep(1, nPos), rep(0, nNeg))

# create distribution of the prior
binwidth = 1/n
theta = seq(from = binwidth/2, to = 1- (binwidth/2), by = binwidth)
a = 1
b = 1
ptheta = dbeta(theta, a, b)

# calculate likelihood, a bernouli distribution given the prior theta
pdatagiventheta = (theta^nPos)*(1-theta)^nNeg

# model data probability distribution
pdata <- sum(pdatagiventheta*ptheta)

# use bayes rule to find pthetagivendata
pthetagivendata <- pdatagiventheta*ptheta/pdata

# plot prior, likelihood and posterior
par(mfrow = c(3,1))
plot(theta, ptheta, type = 'l', lwd = 3, main = 'prior', xlim = c(0, 2*nPos/n))
plot(theta, pdatagiventheta, type = 'l', lwd = 3, main = 'likelihood', xlim = c(0, 2*nPos/n))
plot(theta, pthetagivendata, type = 'l', lwd = 3, main = 'posterior', xlim = c(0, 2*nPos/n))

### part (c)

# create distribution of the prior
binwidth = 0.005
theta = seq(from = binwidth/2, to = 1-(binwidth/2), by = binwidth)
a = 9
b = 5
ptheta <- dbeta(theta, a, b)
ptheta <- ptheta/sum(ptheta)

# calculate likelihood, a bernouli distribution given the prior theta
pdatagiventheta = (theta^nPos)*(1-theta)^nNeg

# model data probability distribution
pdata <- sum(pdatagiventheta*ptheta)

# use bayes rule to find pthetagivendata
pthetagivendata <- pdatagiventheta*ptheta/pdata

# point estimate of theta
estimate <- mean(pthetagivendata)
print(sprintf("point estimate of theta = %s", estimate))

# plot prior, likelihood and posterior
par(mfrow = c(3,1))
plot(theta, ptheta, type = 'l', lwd = 3, main = 'prior')
plot(theta, pdatagiventheta, type = 'l', lwd = 3, main = 'likelihood')
plot(theta, pthetagivendata, type = 'l', lwd = 3, main = 'posterior')



