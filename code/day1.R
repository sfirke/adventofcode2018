# Day 1

library(pacman)
p_load(readr, dplyr, janitor)

x <- read_csv("data/day1_1.txt", col_names = FALSE) %>%
  pull(X1)

sum(x)

# Part 2

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
