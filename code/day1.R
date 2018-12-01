# Day 1

# Setup
library(pacman)
p_load(readr, dplyr, janitor)


# Part 1 ----------------

# Starting with a frequency of zero, what is the resulting frequency after all of the changes in frequency have been applied?
  
x <- read_csv("data/day1_1.txt", col_names = FALSE) %>%
  pull(X1)

sum(x)

# Part 2 ---------------------------------

# What is the first frequency your device reaches twice, continuing in a loop until a cumsum repeats?

x_orig <- x
done <- FALSE
while(!done){
  vals <- cumsum(x)
  dupes <- vals[duplicated(vals)]
  if(length(dupes) > 0){
    done <- TRUE
  }
  x <- c(x, x_orig)
}

first(dupes)
