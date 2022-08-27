### Instructions:
# run all functions to add them to the environment
# then run play_ttt()
# there will be prompts in the console asking for inputs

# Child functions ---------------------------------------------------------

# make the board
draw_board <- function(board_vec){ 
    board <- matrix(board_vec, nrow = 3)
    board
}


# let player do their turn
player_turn <- function(board){
  print(board)
  message("Only enter a number, or that will cause an error and end the game!")
  row <- as.numeric(readline(prompt = "Choose Row"))
  col <- as.numeric(readline(prompt = "Choose Column"))
  if(is.character(row) == T | is.character(col) == T){
    message("Whoops! You accidentally put in some letters.")
  }
  if(row > 3 | col > 3){
    message("Invalid space chosen, try again") # if chose space isn't on the board
    player_turn(board)
  }
  if(board[row,col] == "-"){ # if chosen place is empty
  board[row,col] <- "X"
  print(board)
  } else {
  message("Invalid space chosen, try again")
  player_turn(board)
  }
  
}
  
# computer turn
comp_turn <- function(board){
  board_vec <- as.vector(board)
  random <- as.integer(runif(1,min = 1, max = 9.4999)) # choose a random position
  if(board_vec[random] == "-"){
    board_vec[random] <- "O"
    board <- matrix(board_vec, nrow =3)
  } else {
    comp_turn(board)
  }
}

# choose turn order

order <- function(){
  turn_order <- readline(prompt = "Would you like to go first? (yes or no)")
  if(turn_order == "yes"){
    first <- T
  } else if(turn_order == "no"){
    first <- F
  } else {
    message("Sorry, that was invalid. Make sure to answer with lower case.")
    order()
  }
}

#convert board into easier format for calculations
boardtogame_vec <- function(board, game_vec){
  board_vec <- as.vector(board)
  for(i in 1:length(board_vec)){
    if(board_vec[i] == "X"){
      game_vec[i] <- 3
    } else if(board_vec[i] == "O"){
      game_vec[i] <- 5
    }
  }
  game_vec
} # X's are 3's, O's are 5's, empty is 2 (from initialization)

#check if someone won
check_win <- function(game_vec){
     game_mat <- matrix(game_vec, nrow = 3)
     win <- F
     for(i in 1:3){
       if(prod(game_mat[i,]) == 27 | prod(game_mat[i,]) == 125|
          prod(game_mat[,i]) == 27 | prod(game_mat[,i]) == 125){
         win <- T
       }
     }
     if(prod(diag(game_mat))== 27 | prod(diag(rotate(game_mat)))== 27 |
        prod(diag(game_mat))== 125 | prod(diag(rotate(game_mat)))== 125){
       win <- T
     }
     win
}

rotate <- function(x) t(apply(x, 2, rev)) # used to rotate matrix to check both diagonals

# ask if want to replay game

replay <- function(turn,remainder){
  if(turn %% 2 == remainder){
    reply <- readline(prompt = "You won, Thanks for playing! Would you like to play again? (yes or no)")
  } else {
    reply <- readline(prompt = "You lost :( Thanks for playing! Would you like to play again? (yes or no)")
}
  if(reply == "yes"){
    play_ttt()
  } else if(reply == "no"){
    message("Good Game")
  } else {
    message("Sorry, that was invalid. Make sure to answer with lower case.")
    replay(turn, remainder)
  }
}

replay_draw <- function(){
  reply <- readline("Draw! Thanks for playing! Would you like to play again?")
  if(reply == "yes"){
    play_ttt()
  } else if(reply == "no"){
    message("Good Game")
  } else {
    message("Sorry, that was invalid. Make sure to answer with lower case.")
    replay_draw()
  }
}


# Combined function -----------------------------------------------------------


play_ttt <- function(){
  first <- order()
  if(first == T){
    remainder <- 1
  } else {remainder <- 0}
board_vec <- rep("-",9)
game_vec <- rep(2,9)
board <- draw_board(board_vec)
turn <- 1
for(i in 1:9){
  if(i %% 2 == remainder){
    board <- player_turn(board)
    game_vec <- boardtogame_vec(board, game_vec)
    win <- check_win(game_vec)
    if(win == T){replay(turn, remainder)} else {turn <- turn + 1}
  } else {
    board <- comp_turn(board)
    game_vec <- boardtogame_vec(board, game_vec)
    win <- check_win(game_vec)
    if(win == T){
      print(board) 
      replay(turn, remainder)
    } else {turn <- turn + 1}
  }
  if(turn > 9){
    if(remainder == 0){print(board)}
    replay_draw()
  }
}
}

play_ttt()
