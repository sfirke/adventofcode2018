# let's grow some plants

library(pacman)
p_load(readr, dplyr, tidyr, janitor)

init_state <- "#..#.#..##......###...###" %>%
  strsplit(., "") %>%
  unlist()

padding_l <- 100
padding_r <- 100
init_state <- c(rep(".", padding_l), init_state, rep(".", padding_r))

grow_rules <- read_csv("data/day12_sample.txt", col_names = FALSE) %>%
  pull(X1) %>%
  strsplit(., "") %>%
  as.data.frame() %>%
  clean_names() %>%
  t() %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  select(1:5, result = 10) %>%
  unite(pattern, 1:5, sep = "")

apply_growth_single <- function(i){
  now <- init_state[(i-2):(i+2)]
  outcome <- grow_rules %>%
    filter(pattern == paste0(now, collapse = ""))
  ifelse(nrow(outcome) == 0,
         ".",
         last(outcome$result)
  )
}

# Try it
apply_growth_all <- function(){
  c(rep(".", 2), sapply(3:(length(init_state) - 2), apply_growth), rep(".", 2))
}

# Now loop for generations
for(g in 1:20){
  init_state <- apply_growth_all()
}

hits <- which(init_state == "#") - padding_l - 1 # adjust for my padding and their counting from 0
sum(hits) # 325, works

# Real inputs

init_state <- "..#..####.##.####...#....#######..#.#..#..#.#.#####.######..#.#.#.#..##.###.#....####.#.#....#.#####" %>%
  strsplit(., "") %>%
  unlist()

padding_l <- 100
padding_r <- 100
init_state <- c(rep(".", padding_l), init_state, rep(".", padding_r))


grow_rules <- read_csv("data/day12_real.txt", col_names = FALSE, skip = 2) %>%
  pull(X1) %>%
  strsplit(., "") %>%
  as.data.frame() %>%
  clean_names() %>%
  t() %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  select(1:5, result = 10) %>%
  unite(pattern, 1:5, sep = "")

for(g in 1:20){
  init_state <- apply_growth_all()
}

hits <- which(init_state == "#") - padding_l - 1 # adjust for my padding and their counting from 0
sum(hits) # 2049


### Part 2

# Let's see if there's an obvious sequence so far?

init_state <- "..#..####.##.####...#....#######..#.#..#..#.#.#####.######..#.#.#.#..##.###.#....####.#.#....#.#####" %>%
  strsplit(., "") %>%
  unlist()

padding_l <- 50
padding_r <- 150
init_state <- c(rep(".", padding_l), init_state, rep(".", padding_r))

# They seem to migrate right-ward
hit_sums <- numeric(1000)
for(g in 1:1000){
  init_state <- apply_growth_all()
  hit_sums[g] <- sum(
    which(init_state == "#") - padding_l
  )
  
}

plot(hit_sums) # hey it converges to a couple of values.  What are they?
tail(hit_sums, 50) # repeating over and over, 3891, 4278, 4275

# Would it be different with a different size padding?  I figured the padding in part 1 goes on indefinitely

init_state <- "..#..####.##.####...#....#######..#.#..#..#.#.#####.######..#.#.#.#..##.###.#....####.#.#....#.#####" %>%
  strsplit(., "") %>%
  unlist()

padding_l <- 50
padding_r <- 200
init_state <- c(rep(".", padding_l), init_state, rep(".", padding_r))

# They seem to migrate right-ward
hit_sums_200r <- numeric(1000)
for(g in 1:1000){
  hit_sums_200r[g] <- sum(
    which(init_state == "#") - padding_l
  )
  init_state <- apply_growth_all()
}

plot(hit_sums_200r) # now it converges later
tail(hit_sums_200r, 50) # three repeating values are 5091, 5628, 5625

# There's an interval where it increases constantly, by 46 - presumbly before it hits the right-most boundary
# And that interval is longer in the instance where I've given more right padding - so this would be the true unbounded growth rate
plot(hit_sums_200r[130:150])
hit_sums_200r[130:150]

plot(hit_sums[130:150])
hit_sums[130:150] # matches above

hit_sums[130:150] - lag(hit_sums[130:150]) # it's 46

# So assume this +46 series continues for 50 billion if there was infinite growth allowed on the right

options(scipen = 999)
hit_sums[150] + 46 * (50000000000 - 150)

# will only work once function has become linear at > 130
extend <- function(gens){
  hit_sums[130] + 46 * (gens - 130)
}

extend(140) == hit_sums[140] # yep that works
extend(150) == hit_sums[150]
extend(50000000000) # too low: 2299999997706

# off by one error?
extend(50000000001) # not right either: 2299999997752