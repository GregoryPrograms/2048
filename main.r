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

#Function to move all items in the screen upwards
#   Input - gameWindow: Matrix representing the 2048 grid.
#   Output - gameWindow: Matrix representing the 2048 grid.
moveUp<- function(gameWindow){
    for(x in 1: ncol(gameWindow)){
        trackedV <- c(gameWindow[1,x],1)
        for(y in 2: nrow(gameWindow)){
            if(gameWindow[y,x] == 0){next} #If current square is zero, skip
            else if((gameWindow[y,x] == trackedV[1]) | (trackedV[1] == 0)){ #If we can combine the current square with the tracked one
                gameWindow[trackedV[2],x] <- gameWindow[trackedV[2],x] + gameWindow[y,x]
                gameWindow[y,x] <- 0
                if(trackedV[1] != 0){
                    trackedV[1] <- gameWindow[trackedV[2] + 1,x]
                    trackedV[2] <- trackedV[2] + 1
                    }
                else{
                    trackedV[1] <- gameWindow[trackedV[2], x]
                    }               
                }
            else{ #If current square cannot be combined with the tracked one
                trackedV[2] <- trackedV[2] + 1
                if(trackedV[2] != y){
                    gameWindow[trackedV[2],x] <- gameWindow[y,x]
                    gameWindow[y,x] <- 0
                    }
                trackedV[1] <- gameWindow[trackedV[2],x]
                }
            }
        }
    return(gameWindow)
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
                if(trackedV[1] != 0){
                    trackedV[1] <- gameWindow[y,trackedV[2] + 1]
                    trackedV[2] <- trackedV[2] + 1
                    }
                else{
                    trackedV[1] <- gameWindow[y,trackedV[2]]
                    }                   
                }
            else{
                trackedV[2] <- trackedV[2] + 1
                if(trackedV[2] != x){
                    gameWindow[y,trackedV[2]] <- gameWindow[y,x]
                    gameWindow[y,x] <- 0
                    }
                trackedV[1] <- gameWindow[y,trackedV[2]]
                }
            }
        }
    return(gameWindow)
    }

#Function to move all items in the screen upwards
#   Input - gameWindow: Matrix representing the 2048 grid.
#   Output - gameWindow: Matrix representing the 2048 grid.
moveDown<- function(gameWindow){
    dumbIter <- nrow(gameWindow) - 1
    for(x in 1: ncol(gameWindow)){
        trackedV <- c(gameWindow[nrow(gameWindow),x],nrow(gameWindow))
        for(y in dumbIter : 1){
            if(gameWindow[y,x] == 0){next} #If current square is zero, skip
            else if((gameWindow[y,x] == trackedV[1]) | (trackedV[1] == 0)){ #If we can combine the current square with the tracked one
                gameWindow[trackedV[2],x] <- gameWindow[trackedV[2],x] + gameWindow[y,x]
                gameWindow[y,x] <- 0
                if(trackedV[1] != 0){
                    trackedV[1] <- gameWindow[trackedV[2] - 1,x]
                    trackedV[2] <- trackedV[2] - 1
                    }
                else{
                    trackedV[1] <- gameWindow[trackedV[2], x]
                    }        
                }
            else{ #If current square cannot be combined with the tracked one
                trackedV[2] <- trackedV[2] - 1
                if(trackedV[2] != y){
                    gameWindow[trackedV[2],x] <- gameWindow[y,x]
                    gameWindow[y,x] <- 0
                    }
                trackedV[1] <- gameWindow[trackedV[2],x]
                }
            }
        }
    return(gameWindow)
    }

#Function to move all items in the screen left.
#   Input - gameWindow: Matrix representing the 2048 grid.
#   Output - gameWindow: Matrix representing the 2048 grid. 
moveRight <- function(gameWindow){
    dumbIter <- ncol(gameWindow) - 1
    for(y in 1: nrow(gameWindow)){
        trackedV <- c(gameWindow[y, ncol(gameWindow)],ncol(gameWindow))
        for(x in dumbIter : 1){
            if(gameWindow[y,x] == 0){next}
            else if((gameWindow[y,x] == trackedV[1]) | (trackedV[1] == 0)){
                gameWindow[y,trackedV[2]] <- gameWindow[y,trackedV[2]] + gameWindow[y,x]
                gameWindow[y,x] <- 0
                if(trackedV[1] != 0){
                    trackedV[1] <- gameWindow[y,trackedV[2] - 1]
                    trackedV[2] <- trackedV[2] - 1
                    }
                else{
                    trackedV[1] <- gameWindow[y,trackedV[2]]
                    }               
                }
            else{
                trackedV[2] <- trackedV[2] - 1
                if(trackedV[2] != x){
                    gameWindow[y,trackedV[2]] <- gameWindow[y,x]
                    gameWindow[y,x] <- 0
                    }
                trackedV[1] <- gameWindow[y,trackedV[2]]
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