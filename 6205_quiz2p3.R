### Ex. 3
Lead1996 <- read.table("~/Desktop/Lead1996.txt")
Lead1997 <- read.table("~/Desktop/Lead1997.txt")

## turn data from data frame to a vector with for loop
# initiate blank vector
lead1996.values <- c()
# for loop to add each value to the vector
for (i in 1:length(Lead1996)) {
  lead1996.values <- c(lead1996.values, as.numeric(Lead1996[i]))
}

# initiate blank vector
lead1997.values <- c()
# for loop to add each value to the vector
for (i in 1:length(Lead1997)) {
  lead1997.values <- c(lead1997.values, as.numeric(Lead1997[i]))
}

# 1996
mean.1996 <- mean(lead1996.values)
sd.1996 <- sd(lead1996.values)

# 1997
mean.1997 <- mean(lead1997.values)
sd.1997 <- sd(lead1997.values)


boxplot(list(lead1996.values,lead1997.values), names = c('1976','1977'), 
        main = 'Lead Concentration, San Diego Freeway', horizontal = TRUE, 
        xlab = expression(paste(mu, 'g/m3')), col = "light blue")

print(sprintf("Sample mean of 1976 concentrations: %s", mean.1996))
print(sprintf("Sample standard deviation of 1976 concentrations: %s", sd.1996))
print(sprintf("Sample mean of 1977 concentrations: %s", mean.1997))
print(sprintf("Sample standard deviation of 1977 concentrations: %s", sd.1997))