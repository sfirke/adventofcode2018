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


# Find first zero
# Decrement number two places in front, remove lowest node and tally its sum, repeat

dat <- read_csv("data/day8.txt", col_names = "x")
dat <- as.numeric(strsplit(dat$x, " ")[[1]])
total <- 0

# x <- practice ## for calibrating
x <- dat ## for real, and I'm too lazy to wrap this in a function

first_zero_index <- first(which(x == 0))

# end condition - it errors, but at the right answer
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
i <- 1
parse_nodes <- function(){
  if(i > length(x)) return()
  kid_count <- x[i]
  i <<- i + 1
  meta_count <- x[i]
  i <<- i + 1
  metas <- NA
  
  # if no kids, grab metas and remove them
  if(kid_count == 0){
    metas <- x[i:(i + meta_count - 1)]
    i <<- i + meta_count
  } else {
    kids <- list()
    kid_counter <- kid_count
    while(kid_counter > 0){
      kid_counter <- kid_counter - 1
      kids <- c(kids, list(parse_nodes()))
    }
    
    if(meta_count > 0){
      metas <- x[i:(i + meta_count - 1)]
      i <<- i + meta_count # having this line commented out cost me hours :-/
    }
  }
  
  if(!exists("kids")){
    kids <- NA
  }
  return(list(kid_count = kid_count,
              kids = kids,
              meta_count = meta_count,
              metas = metas))
}

# A nested list tree!  It works!
x <- practice
y <- parse_nodes()

# real data:
i <- 1
x <- dat
actual <- parse_nodes()

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
sum_node_worth(actual)
