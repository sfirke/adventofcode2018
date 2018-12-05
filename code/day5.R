library(pacman)
p_load(readr, stringr, dplyr, purrr)

# Part 1
# Keep eliminating pairs like aA and Aa until all gone

zap_recursive <- function(tx){
  targets <- c(
    paste0(letters, LETTERS),
    paste0(LETTERS, letters)
  )
  res <- str_replace_all(tx, targets, "")
  ifelse(n_distinct(nchar(res)) == 1,
         res,
         zap_recursive(res[which.min(nchar(str_replace_all(tx, targets, "")))])
  )
}

zap_recursive("dabAcCaCBAcCcaDA")
tx <- read_csv("data/day5.txt", col_names = "dat")

nchar(zap_recursive(tx$dat))

# Part 2
# What's the best reduction possible after removing aA or bB etc.

# I had this all set up with map(), but my memory-hogging approach causes stack overflow
# So running it as a loop :-(  Leaving the map code on a list of 1 for demonstration sake

ans <- 100000
for(i in letters[8]){
  x <- tx$dat %>%
    map2(., i, function(x, y) gsub(y, "", x, ignore.case = TRUE)) %>%
    map(zap_recursive) %>%
    map_int(nchar)
  print(x)
}
ans