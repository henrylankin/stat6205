sample_size <- 50

scores <- sample(60:100, 100, replace = TRUE)

score_mean <- mean(scores)
score_sd <- sd(scores)
score_hist <- hist(scores, probability = TRUE, col = 'orange')

mean.sample <- numeric(sample_size)

for (i in 1:1000) {
  x_bar.sample <- sample(scores,sample_size)
  mean.sample[i] <- mean(x_bar.sample)
  # print(mean.sample[i])
}

xbar_hist <- hist(mean.sample, probability = TRUE, col = 'blue', main = sample_size)
mean_xbar <- mean(mean.sample)
sd_xbar <- sd(mean.sample)
print(c(mean_xbar, sd_xbar))