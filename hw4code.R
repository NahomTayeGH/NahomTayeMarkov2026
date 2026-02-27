p <- 0.35
q <- 0.40
s <- 0.25
StartingHand <- 10
Nsims <- 100000

SimGame <- function(StartingHand, p, q, s) {
  Winnings <- StartingHand
  while (Winnings > 0) {
    draw <- runif(1)
    if (draw < s) {
      return(Winnings) 
    } else if (draw < s + p) {
      Winnings <- Winnings + 1 
    } else {
      Winnings <- Winnings - 1 
    }
  }
  return(0) 
}


results <- replicate(Nsims, SimGame(StartingHand, p, q, s))

ExpectedValue <- mean(results)
ProbRetire <- mean(results > 0)
ExpectedValue
ProbRetire
