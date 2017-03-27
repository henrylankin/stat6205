set.seed(1)

# import data from desktop
CommuteAtlanta <- read.delim("~/Desktop/CommuteAtlanta.txt")
time <- CommuteAtlanta$Time

### part (a)
# histogram of the time data
par(mfrow = c(1,1))
hist(time, main = 'Time of commute from Atlanta')

### part (b)
estimate.original <- mean(time)
print(sprintf("original point estimate of time = %s", estimate.original))
sd.original <- sd(time)

### part (c)
# length of each bootstrap sample n, number of bootstrap samples, sig level
n <- length(time)
B <- 1000
alpha <- 0.05

# draw B samples of size n with replacement (really one large sample of size B*n)
bss <- sample(time, n*B, replace = TRUE)

# organize sample into a matrix with nrow=B, bcol=n
bss_matrix <- matrix(bss, nrow = B)

# find mean of each sample (row of the matrix) --> this serves as the sampling distribution of the bootsrap sample means
bs_mean <- rowMeans(bss_matrix)

# sd of boostrap samples
bs_sd <- apply(bss_matrix, 1, sd)

# t-stat for bootstrap method
bs_t <- (bs_mean - estimate.original)/(bs_sd/sqrt(n))

# confidence interval of t-stat
bs_tpercentile <- quantile(bs_t, c(alpha/2, 1-alpha/2))
print(bs_tpercentile)

# create confidence interval of the parameter using the confidence interval of t-stat
lowerlimit <- estimate.original + (bs_tpercentile[1]*sd.original/sqrt(n))
upperlimit <- estimate.original + (bs_tpercentile[2]*sd.original/sqrt(n))
print(sprintf("Lowerlimit = %f, Upperlimit = %f", lowerlimit, upperlimit))