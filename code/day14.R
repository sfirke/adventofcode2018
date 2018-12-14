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

target <- 9
paste0(board[(target + 1):(target + 10)], collapse = "") # 92111343151 was "wrong too high" - whoops one digit too many
