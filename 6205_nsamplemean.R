# 6205 normal sample mean distribution plot
mu <- 50
sigma <- 6
x <- seq(mu-3*sigma, mu+3*sigma, by=1)
plot(x, dnorm(x,50,6), type = 'l', lty = 'dotted', col = 'red', ylim = c(0,0.5), ylab = 'density')

x1 <- seq(50-6, 50+6, by=0.1)
lines(x1, dnorm(x1, 50, 6/3), type = 'l', lty = 'dashed', col = 'blue')
x2 <- seq(50-3, 50+3, by = 0.005)
lines(x2, dnorm(x2, 50, 1), type = "l", lty = "twodash", col = "green")