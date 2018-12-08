# Setup
library(pacman)
p_load(readr, stringr, dplyr, purrr, tibble)

## Part 1 ----------------------
# Find the order of steps based on dependencies
# Would be cooler to treat this as a directed graph, but faster for me not to

dat <- read_delim("data/day7.txt", delim = " ", col_names = FALSE) %>%
  select(prereq_step = X2, step = X8)

remaining_steps <- unique(c(dat$step, dat$prereq_step))
step_set <- remaining_steps

## Psuedocode:
# ID and log alphabetically-first step with no pre-reqs
# Remove that step from steps to build
# Remove those dependency records

# get the alphabetically first step that doesn't have any pre-reqs listed
get_next_step_to_build <- function(){
  first(sort(remaining_steps[!remaining_steps %in% dat$step]))
}

# initialize answer vector
ans <- character(length(step_set))

for(i in seq(26)){
  ans[i] <- get_next_step_to_build()
  remaining_steps <- remaining_steps[!remaining_steps == ans[i]]
  dat <- dat %>%
    filter(!prereq_step == ans[i])
}

paste(ans, collapse = "")

## Part 2 ----------------

## Psuedocode
## While-loop, one second at a time
## Increment time counter
## Check to see if there's a new step available to build
## If so check to see if there's a worker available
## If both yes, assign the worker
## Increment any workers working on steps
## If they finish, adjust steps and worker status accordingly

## Needs:
# data.frame of workers with their current job and job time status 
# already have data.frame of steps
# some helper functions

# maybe simpler than that, do I need to track what job each worker is doing?  How about

#  Take two pseudocode
# While loop, one second at a time
# Store completed steps in strings in a list where each entry is another second's state
# Store total worker availability in a vector, "" ""


dat <- read_delim("data/day7.txt", delim = " ", col_names = FALSE) %>%
  select(prereq_step = X2, step = X8) %>%
  bind_rows(.,
          data.frame(prereq_step = "O", step = "dummy"))

remaining_steps <- unique(c(dat$step, dat$prereq_step))

# overload original function
# now takes the set of steps, returns vector of available letters
get_avail_letters <- function(x){
  sort(unique(c(x$step, x$prereq_step))[!unique(c(x$step, x$prereq_step)) %in% x$step])
}


sec <- 1
worker_avail <- rep(4, 2000) # a single worker can do it in less than 2000 secs
steps_complete <- purrr::map(seq_len(2000), ~dat)
assigned_letters <- "ZZZ"

logging <- integer(0)
logging2 <- character(0)

#while(){
for(i in 1:1300){
  if(worker_avail[sec] > 0){
  availables <- get_avail_letters(steps_complete[[sec]]) 
  if(any(!availables %in% assigned_letters)){
    next_letter <- first(availables[!availables %in% assigned_letters])
    assigned_letters <- c(assigned_letters, next_letter)

    duration <- 60+which(LETTERS == next_letter)
    # remove future completions of this letter
    for(k in (sec+duration):length(steps_complete)){
      steps_complete[[k]] <- steps_complete[[k]] %>%
        filter(!prereq_step == next_letter)
    }
    worker_avail[sec:(sec+duration-1)] <- worker_avail[sec:(sec+duration-1)] - 1

    # for exploring behavior and eventually locating an off-by-two error in Excel
        logging <- c(logging, sec)
    logging2 <- c(logging2, next_letter)
    
  # Reloop with no time penalty if needed, to allow for multiple assignments in one second
  if(any(!availables %in% assigned_letters) & worker_avail[sec] > 0){
    sec <- sec - 1
  }
  }
  }
  sec <- sec + 1

}

# Use this to debug + mock up in Excel, since the order and durations are correct.
cbind(logging, logging2) %>% View

# And the actual real solution
max(logging)+
  60 +
  which(LETTERS == "O") - 1 # minus one b/c my code starts at second 1, not second zero
