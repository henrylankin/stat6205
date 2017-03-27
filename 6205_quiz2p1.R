### Ex. 1
Insurance <- read.table("~/Desktop/Insurance.txt")

## turn data from data frame to a vector with for loop
# initiate blank vector
insurance.values <- c()
# for loop to add each value to the vector
for (i in 1:length(Insurance)) {
  insurance.values <- c(insurance.values, as.numeric(Insurance[i]))
}

## perform summary statistics to get 5-number summary
insurance.summary <- summary(insurance.values)
print(insurance.summary[-4])

## make box blot of the data
boxplot(insurance.values, horizontal = TRUE, main = 'Insurance Losses', xlab = "in 10,000 dollars", col = 'light blue')

## calculate IQR, non-outlier range, non-extreme outlier range
ins.IQR <- insurance.summary[5] - insurance.summary[2]

low <- insurance.summary[2] - 1.5*ins.IQR
high <- insurance.summary[5] + 1.5*ins.IQR

extreme.low <- insurance.summary[2] - 3*ins.IQR
extreme.high <- insurance.summary[5] + 3*ins.IQR

# calculate the outliers and extreme outliers
outliers <- c()
extreme.outliers <- c()
for (value in insurance.values) {
  if (value < low | value > high) {
    outliers <- c(outliers, value)
  }
  if (value < extreme.low | value > extreme.high) {
    extreme.outliers <- c(extreme.outliers, value)
  }
}
outliers.length <- length(outliers)
extreme.outliers.length <- length(extreme.outliers)

print(sprintf("IQR = %f", ins.IQR))
print(sprintf("Range of non-outliers: [%s, %s]", low, high))
print(sprintf("There are %s outliers", outliers.length))
print(outliers)
print(sprintf("Range of non-extreme outliers: [%s, %s]", extreme.low, extreme.high))
print(sprintf("There are %s extreme outliers", extreme.outliers.length))
print(extreme.outliers)