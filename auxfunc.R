computer.Move.Generator <- function(a=22695477,
                                    b=1,
                                    m=2**32,
                                    seed=1234){
random.number <- (a * seed + b) %% m
if (random.number <= 2**31){
  return(c(0, random.number))
} else{
  return(c(1, random.number))
}
}

##max.number.moves <- number.Moves
#initial.seed <- 1234
#current.seed <- initial.seed

#for (i in 1:max.number.moves) {
 # x <- computer.Move.Generator(seed = current.seed)
#  computer.Move <- x[1]
#  new.seed <- x[2]
##  print(paste("computer.move",computer.Move,"new.seed", new.seed))
#  current.seed <- new.seed
#}
