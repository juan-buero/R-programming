my.WD <- getwd()
setwd(my.WD)
source("auxfunc.R")

## Welcome Player to the game

print("Welcome to Human Behavior Prediction by Juan Buero")

repeat{  ##-------- START GAME FROM HERE!!!!----------##
  player.Victories <- 0
  computer.Victories <- 0
  current.seed <- 1234
  human.History <- c()
  human.Move <- NA
  x <- c()
  
## Ask for the difficulty the player wants (easy or difficult)
  difficulty <- as.integer(readline(prompt = "Choose the type of game (1: Easy; 2: Difficult): "))

## Ask how many moves the player wants
  number.Moves <- as.integer(readline(prompt = "Enter the number of moves: "))

  ## Easy Game
 if (difficulty == 1){
  
  for (move in if(number.Moves >= 1) 1:number.Moves else c()) {
    print("-------")

## Get computer move
    x <- computer.Move.Generator(seed = current.seed)
    computer.Move <- x[1]
    new.seed <- x[2]
    print(paste("new.seed for next move will be:", new.seed))
    current.seed <- new.seed

## Get human move
    human.Move <- as.integer(readline(prompt = paste("Choose your move number",move,"(0 or 1): ")))
    
## Computer Wins scenario    
    if (human.Move == computer.Move){
      print(paste("Player =",human.Move," Machine =",computer.Move," - Computer wins!"))
      computer.Victories <- computer.Victories + 1

## Player Wins Scenario      
    }else {
      print((paste("Player =",human.Move," Machine =",computer.Move," - Player wins!")))
      player.Victories <- player.Victories + 1
    }

## Print amount of Stars per Win for Player
    message2print <- "PLAYER: "
    if(player.Victories>0) {
      for (i in 1:player.Victories) {
        message2print <- paste(message2print,"*",sep="")
      }
    }
    print(message2print)
    
## Print amount of Stars per Win for Computer   
    message2print <- "COMPUTER: "
    if(computer.Victories>0) {
      for (i in 1:computer.Victories) {
        message2print <- paste(message2print,"*",sep="")
  }
   }
    print(message2print)
  }
## Display Easy Game Results  
  if (player.Victories > computer.Victories){
    print(paste("Easy game is over, final score: player",player.Victories,
                "-",computer.Victories,"- You Won!"))
  }else if (player.Victories == computer.Victories){
    print(paste("Easy game is over, final score: player",player.Victories,
                "-",computer.Victories,"- computer - it's a TIE!"))
  }else{
    print(paste("Easy game is over, final score: player",player.Victories,
                "-",computer.Victories,"- computer - the COMPUTER Won!"))
  }
anotherRound <- as.integer(readline(prompt = "Play again? 0: No || 1: Yes--> "))

if (anotherRound == 0){
  break
}

  
##----------------------------------------------------------------------------##
  
## Difficult Game
} else {
  
  for (move in if(number.Moves >=1) 1:number.Moves else c()) {
    print("-------")
    if (move == 1){
      ## Get Random computer move
      x <- computer.Move.Generator(seed = current.seed)
      computer.Move <- x[1]
      new.seed <- x[2]
      print(paste("new.seed for next move will be:", new.seed))
      current.seed <- new.seed
      previous.Human.Move <- NA
    } else {
      ## Computer Chooses based on human behavior
      previous.Human.Move <- human.History[move-1]
      if (previous.Human.Move == 0){
        if (sum(throw10) > sum(throw00)){
          computer.Move <- 1
        } else if (throw10 < throw00){
          computer.Move <- 0
        } else if(throw10 == throw00){
          x <- computer.Move.Generator(seed = current.seed)
          computer.Move <- x[1]
          new.seed <- x[2]
          print(paste("new.seed for next move will be:", new.seed))
          current.seed <- new.seed
        }
        
      } else if(previous.Human.Move == 1){
        if (sum(throw11) > sum(throw01)){
          computer.Move <- 1
        } else if (sum(throw11) < sum(throw01)){
          computer.Move <- 0
        } else if(throw11 == throw01){
          x <- computer.Move.Generator(seed = current.seed)
          computer.Move <- x[1]
          new.seed <- x[2]
          print(paste("new.seed for next move will be:", new.seed))
          current.seed <- new.seed
        }
      }
    }
    
    
    ## Get human move & record throw
    human.Move <- as.integer(readline(prompt = paste("Choose your move number",move,"(0 or 1): ")))
    human.History <- c(human.History, human.Move)
    
    if (move == 1){
      throw00 <- 0
      throw01 <- 0
      throw10 <- 0
      throw11 <- 0
    } else {
      if (human.Move == 0 && previous.Human.Move == 0){
        throw00 <- throw00 + 1
      } else if (human.Move == 0 && previous.Human.Move == 1){
        throw01 <- throw01 + 1
      } else if(human.Move == 1 && previous.Human.Move == 0){
        throw10 <- throw10 + 1
      } else{
        throw11 <- throw11 + 1
      }
    }
    
  
    ## Computer Wins scenario    
    if (human.Move == computer.Move){
      print(paste("Player =",human.Move," Machine =",computer.Move," - Computer wins!"))
      computer.Victories <- computer.Victories + 1
      
      ## Player Wins Scenario      
    }else {
      print((paste("Player =",human.Move," Machine =",computer.Move," - Player wins!")))
      player.Victories <- player.Victories + 1
    }
    
    ## Print amount of Stars per Win for Player
    message2print <- "PLAYER: "
    if(player.Victories>0) {
      for (i in 1:player.Victories) {
        message2print <- paste(message2print,"*",sep="")
      }
    }
    print(message2print)
    
    ## Print amount of Stars per Win for Computer   
    message2print <- "COMPUTER: "
    if(computer.Victories>0) {
      for (i in 1:computer.Victories) {
        message2print <- paste(message2print,"*",sep="")
      }
    }
    print(message2print)
  }
  
  ## Display Difficult Game Results  
  if (player.Victories > computer.Victories){
    print(paste("Difficult game is over, final score: player",player.Victories,
                "-",computer.Victories,"- You Won!"))
  }else if (player.Victories == computer.Victories){
    print(paste("Difficult game is over, final score: player",player.Victories,
                "-",computer.Victories,"- computer - it's a TIE!"))
  }else{
    print(paste("Difficult game is over, final score: player",player.Victories,
                "-",computer.Victories,"- computer - the COMPUTER Won!"))
  }


## Ask if player want to play again
  anotherRound <- as.integer(readline(prompt = "Play again? 0: No || 1: Yes--> "))
  
  if (anotherRound == 0){
    break
  }
}
}

