library(pacman)
p_load(readr, dplyr)


dat <- scan("data/day13.txt", what = "", sep = "x")

area <- matrix("", 150, 150)

# get data into matrix form
for(i in 1:150){
  separated <- unlist(strsplit(dat[i], "")[1])
  area[i, ] <- separated
}

# find the carts, store their coords, and index them to keep track of their direction and intersection behavior
carts <- rbind(
  which(d == "^" , arr.ind = TRUE) %>% cbind(dir = "up"),
  which(d == "<" , arr.ind = TRUE) %>% cbind(dir = "left"),
  which(d == ">" , arr.ind = TRUE) %>% cbind(dir = "right"),
  which(d == "v" , arr.ind = TRUE) %>% cbind(dir = "down")
) %>%
  as_tibble() %>%
  mutate_at(1:2, as.numeric) %>%
  arrange(row, col) %>%
  mutate(next_turn = "left")

intersection_cycle <- tibble(
  current_dir = c("left", "straight", "right"),
  next_dir = c("straight", "right", "left")
)

# movement function: use direction and current space to move, update direction and intersection behavior if needed
# update coords

i <- 1
if(carts$dir[i] == "left") { carts$col[i] <- carts$col[i] - 1}
if(carts$dir[i] == "right") { carts$col[i] <- carts$col[i] + 1}
if(carts$dir[i] == "down") { carts$row[i] <- carts$row[i] + 1}
if(carts$dir[i] == "up") { carts$row[i] <- carts$row[i] - 1}

## THIS IS BOTCHED, b/c left/right/straight turn interacts with compass direction they are moving
## Could address via a multivariate join, e.g., if current = U and turn = L, then dir = L
cur_action <- area[carts$row[1], carts$row[1]]
if(cur_action == "+"){
  carts$dir[i] <- carts$next_turn[i] ## FIX THIS
  carts$next_turn[i] <- intersection_cycle$next_dir[intersection_cycle$current_dir == carts$next_turn[i]]
}

# from top down, l-to-r, move the carts and check for collisions

