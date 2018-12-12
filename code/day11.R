# setup
library(pacman)
p_load(dplyr, purrr)

options(scipen = 999) # so 400000 doesn't turn into 4e+05

set_power_levels <- function(serial){
  
  dat <- matrix(NA_integer_, 300, 300)
  
  power_level <- function(x, y, serial){
    rack_id <- x + 10
    power_lvl <- rack_id * y
    power_lvl <- power_lvl + serial
    power_lvl <- power_lvl * rack_id
    power_lvl <- paste0("000", power_lvl)
    power_lvl <- substr(power_lvl, start = (nchar(power_lvl) - 2), stop = (nchar(power_lvl) - 2))
    power_lvl <- as.numeric(power_lvl) - 5
    power_lvl
  }
  
  for(x in 1:300){
    for(y in 1:300){
      dat[x, y] <- power_level(x, y, serial)
    }
  }
  
  dat
}

powers <- set_power_levels(3999)

# crawl and get best 3x3 grid
sum_zone <- function(x, y){
  sum(powers[x:(x + 2), y:(y + 2)])
}

max_zone <- 0
max_coords <- c(0,0)
for(j in 1:298){
  for(k in 1:298){
    if(sum_zone(j, k) > max_zone){
      max_zone <- sum_zone(j, k)
      max_coords <- c(j, k)
    }
  }
}
