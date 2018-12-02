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
