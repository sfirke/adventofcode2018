library(pacman)

e1_place <- 1
e2_place <- 2

board <- c(3, 7)

get_new_elf_place <- function(current, num_moves){
  num_moves <- num_moves %% length(board)
  if((current + num_moves) >= length(board)){
    num_moves <- num_moves - (length(board) - current) - 1
    current <- 1
  }
  current + num_moves + 1
}

update_board <- function(rep1, rep2){
  total <- rep1 + rep2
  if(nchar(total) == 2){
    board <<- c(board, as.numeric(substr(total, 1, 1)), as.numeric(substr(total, 2, 2)))
  } else{
    board <<- c(board, total)
  }
}

for(i in 1:077201 + 10){
  update_board(board[e1_place], board[e2_place])
  e1_place <- get_new_elf_place(e1_place, board[e1_place])
  e2_place <- get_new_elf_place(e2_place, board[e2_place])
}

target <- 077201
paste0(board[(target + 1):(target + 10)], collapse = "") # 92111343151 was "wrong too high" - whoops one digit too many


# Part 2

target <- as.character(target)
# find position
# test:
target <- 59414
boundary <- numeric(0)
for(j in 1:length(board)){
  if(paste0(board[j:(j+nchar(target)-1)], collapse = "") == target){
    boundary <- j - 1
    break
  }
}
boundary # yep 2018

# Actual. Going to need a bigger run of numbers, hope I can brute force this
# Let's set it to 10MM and I'll go do work for a while and come back to it.
# I could rewrite the code to study the loop as it runs, but this is faster in terms of people time.
# I guess growing the board vector with c() ... will be quite inefficient but again, don't want to refactor to
  # preallocate "board" with NA and keep an index of where the technical end of the vector is at any moment 

for(i in 1:10000000){
  update_board(board[e1_place], board[e2_place])
  e1_place <- get_new_elf_place(e1_place, board[e1_place])
  e2_place <- get_new_elf_place(e2_place, board[e2_place])
}

target <- 077201
boundary <- numeric(0)
for(j in 1:length(board)){
  if(paste0(board[j:(j+nchar(target)-1)], collapse = "") == target){
    boundary <- j - 1
    break
  }
}
boundary