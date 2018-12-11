library(pacman)
p_load(readr, dplyr, purrr)

practice <- "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
practice <- as.numeric(strsplit(practice, " ")[[1]])


# Part 1
# Can I avoid building a data structure and instead just collapse metadata into sums?

# If there's a zero, replace X Y 0 Z 1 2 3 with X-1, Y+1, and Z many numbers collapsed
# Eg 2 2 0 3 5 6 7 becomes 1 3 18

# 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
# 1 4 33 0 2 99 2 1 1 2
# 0 5 33 101 1 1 2
# 33 + 101 + 1 + 1 + 2 = 138


# Helper function
# Takes arbitrary length vector of #s beginning with zero, returns sum
# 0 3 10 11 12 1 1 would return 33 1 1
zero_trimmer <- function(x){
  end_sum <- x[2] + 2
  c(
    sum(x[3:end_sum]),
  x[(end_sum + 1):length(x)]
    )
}

# Takes zero-beginning vector, sums the relevant meta values
sum_metas <- function(x){
  end_sum <- x[2] + 2
  sum(x[3:end_sum])
}

# Find first zero
# Decrement number two places in front, remove lowest node and tally its sum, repeat

dat <- read_csv("data/day8.txt", col_names = "x")
dat <- as.numeric(strsplit(dat$x, " ")[[1]])
total <- 0

# x <- practice ## for calibrating
x <- dat ## for real, and I'm too lazy to wrap this in a function

first_zero_index <- first(which(x == 0))

# is this the right end condition?
while(first_zero_index + 1 + x[first_zero_index + 1] <= length(x)){

first_zero_index <- first(which(x == 0))
if(first_zero_index > 1){
  x[first_zero_index - 2] <- x[first_zero_index - 2] - 1
}
total <- total + sum_metas(x[first_zero_index:length(x)])
x <- c(x[1:(first_zero_index - 1)],
       x[(first_zero_index + 2 + x[(first_zero_index + 1)]):length(x)])
}

## Part 2 ---------------------

# Create a function that actually produces a nested list?

list(kids = 2, metas = 3)

parse_nodes <- function(x){
  kid_count <- x[1]
  meta_count <- x[2]
  
  print(paste0("kid_count: ", kid_count))
  print(paste0("meta_count: ", meta_count))
  
  len <- 2
  
  metas <- NA
  
  # if no kids, grab metas and remove them
  if(kid_count == 0){
    metas <- x[3:(2 + meta_count)]
    print(paste0("metas: ", paste0(metas, collapse = ", ")))
    x <- x[(2 + meta_count + 1):length(x)]
  } else {
    kids <- list()
    kid_counter <- kid_count
    x <- x[3:length(x)]
    while(kid_counter > 0){
      kid_counter <- kid_counter - 1
      kids <- c(kids, list(parse_nodes(x)))
      len <- len + kids[[length(kids)]]$meta_count
      x <- x[(len + 1):length(x)]
    }
    
    if(meta_count > 0){
      metas <- x[1:meta_count]
      print(paste0("metas: ", paste0(metas, collapse = ", ")))
    }
  }
  
  if(!exists("kids")){
    kids <- NA
  }
  
  return(list(kid_count = kid_count,
              kids = kids,
              meta_count = meta_count,
              metas = metas,
              length = len))
}

# A nested list tree!
y <- parse_nodes(practice)

# function to calculate the sum per the instructions

sum_node_worth <- function(x){
  # if terminal node, return sum of meta values
  if(x$kid_count == 0){
    return(sum(x$metas))
  } else {
    valid_metas <- x$metas[x$metas <= length(x$kids)]
    if(length(valid_metas) == 0){ # abort if only out of bounds indices
      return(0)
    }
    return( # get recursive
      sum(
        map_dbl(valid_metas, function(index) sum_node_worth(x$kids[[index]]))
      )
    )
  }
}

sum_node_worth(y) # 66, checks out

dat %>%
  parse_nodes() %>%
  sum_node_worth()
