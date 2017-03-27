
sampling.distribution <- function(sample_size) {
  # set sample size to n
  n <- sample_size
  
  # create blank
  mean.sample <- c()
  
  # take 1000 samples from the uniform distribution and find their sample mean, add sample mean to mean.sample
  for (i in 1:1000) {
    
    xbar.sample <- runif(n)
    mean.sample <- c(mean.sample, mean(xbar.sample))
    #print(mean.sample[i])
  }
  
  # graph the sample mean distribution, calculate mean and sd of sample means
  xbar_hist <- hist(mean.sample, probability = TRUE, col = 'blue', main = n)
  mean_xbar <- mean(mean.sample)
  sd_xbar <- mean(mean.sample)
  
  # print mean and sd of samples
  print(sprintf("mean of sample means: %f, sd of sample means: %f", mean_xbar, sd_xbar))
  
  return()
}




