# Day 3

# Setup
library(pacman)
p_load(readr, dplyr, janitor, tidyr, stringr, purrr)


# Part 1 ----------------

# How many square inches are claimed 2+ times?
# Layer them all as matrices!

d <- read_delim("data/day3_1.txt", delim = "@", col_names = FALSE) %>%
  rename(id = X1) %>%
  separate(X2, into = c("x", "y", "width", "height"), sep = "[@|,|:|x]") %>%
  mutate_all(parse_number)

create_matrix <- function(x, y, w, h){
  m <- matrix(rep(0, 1000000), 1000, 1000)
  right <- x + w
  bottom <- y + h
  m[(y + 1):(bottom), ((x + 1):(right))] <- 1
  m
}

ans <- matrix(rep(0, 1000000), 1000, 1000)

# This ran out of memory so will have to do them sequentially:
# all_mat <- pmap(list(d$x, d$y, d$width, d$height), create_matrix)

for(i in seq(nrow(d))){
  ans <- ans + create_matrix(
    d$x[i],
    d$y[i],
    d$width[i],
    d$height[i]
  )
}

sum(ans > 1)
