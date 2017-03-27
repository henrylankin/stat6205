## bootstrap step 1
# original data
comp_scores <- c(60, 76, 26, 90, 81, 75, 95, 98, 38, 73, 90, 46, 41, 83, 100, 85, 76, 69, 91, 78)

# calculate original mean
sample_mean <- mean(comp_scores)
print(sprintf("Original mean: %f", sample_mean))

# check normality of original sample
qqnorm(comp_scores)
qqline(comp_scores)
shapiro.test(comp_scores)

# length of each bootstrap sample n, number of bootstrap samples, sig level
n <- length(comp_scores)
B <- 1000
alpha <- 0.05

## bootstrap step 3 - mean
# draw B samples of size n with replacement (really one large sample of size B*n)
bss <- sample(comp_scores, B*n, replace = TRUE)

# organize sample into a matrix with nrow=B, bcol=n
bss_matrix <- matrix(bss, nrow = B)

## bootstrap step 3
# find mean of each sample (row of the matrix) --> this serves as the sampling distribution of the bootsrap sample means
bs_mean <- rowMeans(bss_matrix)

# histogram of boostrap sample means distributions
hist(bs_mean)

# confidence interval of true parameter from boostrap samples
bci <- quantile(bs_mean, c(alpha/2, 1 - alpha/2))
print(bci)

# sd of original sample
sample_sd <- sd(comp_scores)

## bootstrap step 3 - sd
# sd of boostrap samples
bs_sd <- apply(bss_matrix, 1, sd)

## bootstrap step 4
# t-stat for bootstrap method
bs_t <- (bs_mean - sample_mean)/(bs_sd/sqrt(n))

# confidence interval of t-stat
bs_tpercentile <- quantile(bs_t, c(alpha/2, 1-alpha/2))
print(bs_tpercentile)

# create confidence interval of the parameter using the confidence interval of t-stat
lowerlimit <- sample_mean + (bs_tpercentile[1]*sample_sd/sqrt(n))
upperlimit <- sample_mean + (bs_tpercentile[2]*sample_sd/sqrt(n))
print(sprintf("Lowerlimit = %f, Upperlimit = %f", lowerlimit, upperlimit))

## hypothesis testing
### H0: mu = 80; Ha: mu < 80
# actual t-value of sample
t_actual <- (sample_mean - 80)/(sample_sd/sqrt(n))

# p_value using the bootstrap t's that are less than the actual t
p_value <- mean(bs_t < t_actual)
print(sprintf("p-value = %f", p_value))
