# Setup
library(pacman)
p_load(readr, stringr, dplyr, purrr, tibble)

# Part 1
# Find station that occupies the most neighboring territory
# Create and populate a matrix with station ID #s ?

stations <- tribble(
  ~x, ~y,
  1, 1,
  1, 6,
  8, 3,
  3, 4,
  5, 5,
  8, 9
) %>%
  mutate(id = 1:6)


# Manhattan distance calculator
walk_distance <- function(x1, y1, x2, y2){
  abs(x1-x2) + abs(y1-y2)
}
# given a coordinate pair, returns ID of closest station
label_coord <- function(x, y){
  dists <- pmap_dbl(list(x, y, stations$x, stations$y), walk_distance)
  # If a tie return NA
  ifelse(sum(dists == min(dists)) > 1,
         NA,
         stations$id[which.min(dists)]
  )
}

stations <- read_csv("data/day6.txt", col_names = c("x", "y")) %>%
  mutate(id = row_number())


# Deal with infinite reach by creating a big matrix, then there should be a big gap between infinites and not

mapped <- matrix(NA, 1000, 1000)
for(x in 1:1000){
  for(y in 1:1000){
    mapped[x, y] <- label_coord(x, y)
  }
}

res <- table(mapped)

# Hrm there's not an obvious cutoff, I'd bet on stations 24, 6, 25, 40 but not sure which.
# Okay ID the infinite stations by mapping a new super-outer ring and removing those stations

inf_stations <- map2_dbl(
  c(-10000:10000, rep(10000, 20000), -10000:10000, rep(-10000, 20000)),
  c(rep(10000, 20000), -10000:10000, rep(-10000, 20000), -10000:10000),
  label_coord
) %>%
  unique

# Get answer.  And I was wrong in my suspicious above, ha!
res[!names(res) %in% inf_stations] %>%
  sort %>%
  last

# Part 2

safe_coord <- function(x, y, dist_lim = 10000){
  dists <- pmap_dbl(list(x, y, stations$x, stations$y), walk_distance)
  sum(dists) < dist_lim
}

safe_mapped <- matrix(NA, 400, 400)
for(x in 1:400){
  for(y in 1:400){
    mapped[x, y] <- safe_coord(x, y)
  }
}

sum(safe_mapped)
