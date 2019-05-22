#Function to initialize the game window.
#   Input - NUM_SQUARES: number of squares (int, needs to be a square, treated as constant)
#   Output - matrix representing the 2048 grid
initGameWindow <- function(NUM_SQUARES){
    gameWindow <- matrix(0L, nrow = sqrt(NUM_SQUARES), ncol = sqrt(NUM_SQUARES))
    gameWindow = insertRand(gameWindow)
    gameWindow = insertRand(gameWindow)
}

#Function to insert either 2 or 4 into the graph. returns gameWindow
#   Input - matrix representing the 2048 grid
#   Output - same as input
insertRand <- function(gameWindow){
    zeroPos = sample(which(gameWindow == 0),1)
    gameWindow[zeroPos] = sample(c(2,4), 1)
    gameWindow
}

#Function tracking a 'step' in the game.
#Tries to perform a move requested by user. If move is impossible, returns to query another move.
#If move is performed, adds a 2 or 4 to an empty square, checks for end game, returns to query.
#   Input - gameWindow: Matrix representing the 2048 grid.
#           moveDirect: string deciding direction of the move
#
#   Output - gameWindow: Matrix representing the 2048 grid.
gameStep <- function(gameWindow, moveDirect){
    
}
print(initGameWindow(16))