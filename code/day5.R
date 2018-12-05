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
  ifelse(all(nchar(res) == nchar(tx)),
         res,
         zap_recursive(res[which.min(nchar(str_replace_all(tx, targets, "")))])
  )
}

zap_recursive("dabAcCaCBAcCcaDA")
tx <- read_csv("data/day5.txt", col_names = "dat")

nchar(zap_recursive(tx$dat))

# Part 2
# What's the best reduction possible after removing a and A or b and B or c and C etc.

# I had this all set up with map(), but my memory-hogging approach causes stack overflow
# Update: I tried converting from a list to a loop and the problem isn't the list, it's that an individual case - replacing "h" - causes the recursive function to exceed memory, going too deep / too many reduction cycles
# So I have to convert it to a loop :-(  How inelegant.
# At least I got to go back to a purrr list!

zap_loop <- function(tx){
  all_same <- FALSE
  targets <- c(
    paste0(letters, LETTERS),
    paste0(LETTERS, letters)
  )
  while(!all_same){
    res <- str_replace_all(tx, targets, "")
    all_same <- (n_distinct(nchar(res)) == 1)
    tx <- res[which.min(nchar(str_replace_all(tx, targets, "")))]
  }
  nchar(res)[1]
}


tx$dat %>%
  map2(., letters, function(x, y) gsub(y, "", x, ignore.case = TRUE)) %>%
  map_int(zap_loop) %>%
  min
