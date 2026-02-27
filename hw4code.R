p <- 0.35
q <- 0.40
s <- 0.25
StartingHand <- 10
n_sims <- 100000

simulate_game <- function(StartingHand, p, q, s) {
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


results <- replicate(n_sims, simulate_game(StartingHand, p, q, s))

numerical_expected_value <- mean(results)
prob_retire <- mean(results > 0)
numerical_expected_value
prob_retire
