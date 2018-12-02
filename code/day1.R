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


# A solution I thought of later - no need for a loop since
# the answer doesn't ask how many iterations it took

x %>%
  rep(1000) %>% # probably excessive but the calculations are fast
  cumsum %>%
  .[duplicated(.)] %>% # ugly but I can't stop piping
  unique() %>%
  first
