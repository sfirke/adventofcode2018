# Day 2

# Setup
library(pacman)
p_load(readr, dplyr, janitor, tidyr, stringr, purrr)


# Part 1 ----------------

# Count the number of strings with a letter that appears twice;
# Count the number of strings with a letter that appears thrice;
# Multiply these two numbers

x <- read_csv("data/day2_1.txt", col_names = FALSE) %>%
  pull(X1)

split_chars <- function(x){
  unlist(str_split_fixed(x, pattern = "", n = nchar(x)))
}

repeats <- x %>%
  map(split_chars) %>%
  map(as.vector) %>%
  map(tabyl) %>% # letter frequencies
  map_df(tabyl, n) %>% # count the frequencies
  tabyl(n) # count how often "2" or "3" appeared (ignore how many times it did)

repeats$n_n[2] * repeats$n_n[3]

# Part 2

# function takes two vectors, compares to see if they meet answer condition
# if yes, prints answer
reveal_answer <- function(x, y){
  if(
    sum(x == y) == (length(x) - 1)
  ) {
    print(paste0(x[x == y], collapse = ""))
  }
}

# test
reveal_answer(c("a", "b", "c", "d", "e"),
              c("a", "b", "c", "d", "f"))

vecs <- x %>%
  map(split_chars) %>%
  map(as.vector)

walk(vecs, function(x) map(vecs, function(y) reveal_answer(x, y)))
