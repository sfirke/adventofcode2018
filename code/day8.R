library(pacman)
p_load(readr, dplyr)

practice <- "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
practice <- as.numeric(strsplit(practice, " ")[[1]])


# Part 1
# Can I avoid building a data structure and instead just collapse metadata into sums?

# If there's a zero, replace X Y 0 Z 1 2 3 with X-1, Y+1, and Z many numbers collapsed
# Eg 2 2 0 3 5 6 7 becomes 1 3 18

# 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
# 1 4 33 0 2 99 2 1 1 2
# 0 5 33 101 1 1 2
# 33 + 101 + 1 + 1 + 2 = 138


# Helper function
# Takes arbitrary length vector of #s beginning with zero, returns sum
# 0 3 10 11 12 1 1 would return 33 1 1
zero_trimmer <- function(x){
  end_sum <- x[2] + 2
  c(
    sum(x[3:end_sum]),
  x[(end_sum + 1):length(x)]
    )
}

# Takes zero-beginning vector, sums the relevant meta values
sum_metas <- function(x){
  end_sum <- x[2] + 2
  sum(x[3:end_sum])
}

# Find first zero
# Decrement number two places in front, remove lowest node and tally its sum, repeat

dat <- read_csv("data/day8.txt", col_names = "x")
dat <- as.numeric(strsplit(dat$x, " ")[[1]])
total <- 0

# x <- practice ## for calibrating
x <- dat ## for real, and I'm too lazy to wrap this in a function

first_zero_index <- first(which(x == 0))

# is this the right end condition?
while(first_zero_index + 1 + x[first_zero_index + 1] <= length(x)){

first_zero_index <- first(which(x == 0))
if(first_zero_index > 1){
  x[first_zero_index - 2] <- x[first_zero_index - 2] - 1
}
total <- total + sum_metas(x[first_zero_index:length(x)])
x <- c(x[1:(first_zero_index - 1)],
       x[(first_zero_index + 2 + x[(first_zero_index + 1)]):length(x)])
}

