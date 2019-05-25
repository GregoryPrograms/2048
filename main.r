#Function to initialize the game window.
#   Input - NUM_SQUARES: number of squares (int, needs to be a square, treated as constant)
#   Output - matrix representing the 2048 grid
initGameWindow <- function(NUM_SQUARES){
    gameWindow <- matrix(0L, nrow = sqrt(NUM_SQUARES), ncol = sqrt(NUM_SQUARES))
    gameWindow <- insertRand(gameWindow)
    gameWindow <- insertRand(gameWindow)
}

#Function to insert either 2 or 4 into the graph. returns gameWindow
#   Input - matrix representing the 2048 grid
#   Output - same as input
insertRand <- function(gameWindow){
    zeroPos <- sample(which(gameWindow == 0),1)
    gameWindow[zeroPos] <- sample(c(2,4), 1)
    gameWindow
}
#Function to move all items in the screen left.
#   Input - gameWindow: Matrix representing the 2048 grid.
#   Output - gameWindow: Matrix representing the 2048 grid. 
moveLeft <- function(gameWindow){
    for(y in 1: nrow(gameWindow)){
        trackedV <- c(gameWindow[y, 1],1)
        for(x in 2: ncol(gameWindow)){
            if(gameWindow[y,x] == 0){next}
            else if((gameWindow[y,x] == trackedV[1]) | (trackedV[1] == 0)){
                gameWindow[y,trackedV[2]] <- gameWindow[y,trackedV[2]] + gameWindow[y,x]
                gameWindow[y,x] <- 0
                trackedV[1] <- gameWindow[y,trackedV[2] + 1]
                trackedV[2] <- trackedV[2] + 1           
                }
            else{
                trackedV[1] <- gameWindow[y,trackedV[2] + 1]
                trackedV[2] <- trackedV[2] + 1
                if(trackedV[2] != x){
                    gameWindow[y,trackedV[2]] <- gameWindow[y,trackedV[2]] + gameWindow[y,x]
                    gameWindow[y,x] <- 0
                    }
                }
            }
        }
    return(gameWindow)
    }
#Function tracking a 'step' in the game.
#Tries to perform a move requested by user. If move is impossible, returns to query another move.
#If move is performed, adds a 2 or 4 to an empty square, checks for end game, returns to query.
#   Input - gameWindow: Matrix representing the 2048 grid.
#           moveDirect: string deciding direction of the move
#
#   Output - gameWindow: Matrix representing the 2048 grid.
gameStep <- function(gameWindow, moveDirect){
    if(moveDirect == "W"){gameWindowN <- moveUp(gameWindow)}
    else if(moveDirect == "A"){gameWindowN <- moveLeft(gameWindow)}
    else if(moveDirect == "S"){gameWindowN <- moveDown(gameWindow)} 
    else if(moveDirect == "D"){gameWindowN <- moveRight(gameWindow)}
        if(identical(gameWindow ,gameWindowN)){
            return(gameWindow)
            }
        else{
            gameWindowN <- insertRand(gameWindowN)
            return(gameWindowN)
        }
    }
}
main <- function(){
    gameWindow <- initGameWindow(16)
    print(gameWindow)
    while(1){
        moveDirect <- readline(prompt= "Your move yugiboi: ")
        gameWindow <-(gameStep(gameWindow, moveDirect ))
        print(gameWindow)
    }
}
main()