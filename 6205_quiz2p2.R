### Ex. 2
Rearview <- read.table("~/Desktop/Rearview.txt")

## turn data from data frame to a vector with for loop
# initiate blank vector
rearview.values <- c()
# for loop to add each value to the vector
for (i in 1:length(Rearview)) {
  rearview.values <- c(rearview.values, as.numeric(Rearview[i]))
}

# calculate break width for histogram
break.width <- (max(rearview.values) - min(rearview.values))/10

# while loop to create list of breaks using break.width for 10 classes
temp <- min(rearview.values)
breaks <- c(temp)
while (temp <= max(rearview.values)) {
  temp <- temp + break.width
  breaks <- c(breaks, temp)
}

# create histogram with 10 classes
rearview.hist <- hist(rearview.values, main = "Rearview Weights", xlab = "weight", 
                      breaks = breaks, xlim = c(min(rearview.values), max(rearview.values)), col = 'light blue')