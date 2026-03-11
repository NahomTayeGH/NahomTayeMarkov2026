###Problem 2 Part C
ParamA <- 0.04
ParamB <- 0.16
ParamK <- 0.1
States <- 1:5
NumSteps <- 10^6

GetProbRight <- function(N) { 
  if(N < 5) ParamK * exp(ParamA * N) else 0 
}

GetProbLeft <- function(N) { 
  if(N > 1) ParamK * exp(ParamB * (N - 1)) else 0 
}


StateHistory <- numeric(NumSteps)
CurrentState <- 3 

for (I in 1:NumSteps) {
  P_Right <- GetProbRight(CurrentState)
  P_Left <- GetProbLeft(CurrentState)
  P_Stay <- 1 - P_Right - P_Left
  
  
  PossibleMoves <- c(CurrentState - 1, CurrentState, CurrentState + 1)
  MoveProbabilities <- c(P_Left, P_Stay, P_Right)
  
 
  CurrentState <- sample(PossibleMoves, size = 1, prob = MoveProbabilities)
  StateHistory[I] <- CurrentState
}

SimulatedDist <- table(factor(StateHistory, levels = 1:5)) / NumSteps

## Part D
TransitionMatrix <- matrix(0, nrow = 5, ncol = 5)
for(Row in 1:5) {
  if(Row < 5) TransitionMatrix[Row, Row + 1] <- GetProbRight(Row)
  if(Row > 1) TransitionMatrix[Row, Row - 1] <- GetProbLeft(Row)
  TransitionMatrix[Row, Row] <- 1 - GetProbRight(Row) - GetProbLeft(Row)
}

EigenResults <- eigen(t(TransitionMatrix))
EigenVectorDist <- Re(EigenResults$vectors[, 1]) 
EigenVectorDist <- EigenVectorDist / sum(EigenVectorDist)


DetailedWeights <- numeric(5)
DetailedWeights[1] <- 1
for(Step in 1:4) {
  DetailedWeights[Step + 1] <- DetailedWeights[Step] * (GetProbRight(Step) / GetProbLeft(Step + 1))
}
DetailedBalanceDist <- DetailedWeights / sum(DetailedWeights)

PlotData <- rbind(SimulatedDist, EigenVectorDist, DetailedBalanceDist)

barplot(PlotData, 
        beside = TRUE, 
        names.arg = States,
        col = c("steelblue", "salmon", "palegreen3"),
        main = "Stationary Distribution Comparison",
        xlab = "States (n)", 
        ylab = "Probability P(n)",
        legend.text = c("Simulated (c)", "Eigenvector (b)", "Detailed Bal (a)"),
        args.legend = list(x = "topright"))

## Problem 3 Part B
ParamA <- 0.99


GetEigenSystem <- function(AVal) {
 
  P <- matrix(c(
    1 - AVal,   AVal,       0,           
    AVal,       0,           1 - AVal,   
    0,           1 - AVal,   AVal       
  ), nrow = 3, byrow = TRUE)
  

  MatrixA <- t(P)
  
  EigenResults <- eigen(MatrixA)
  return(EigenResults)
}

Result <- GetEigenSystem(ParamA)
Result$values
Result$vectors

#Part E
ParamA <- 0.99
NumChains <- 1000  
MaxTime <- 300
InitialState <- 1

EigenSys <- GetEigenSystem(ParamA)
Pi <- Re(EigenSys$vectors[, 1])
Pi <- Pi / sum(Pi)

StateMatrix <- matrix(0, nrow = NumChains, ncol = MaxTime + 1)
StateMatrix[, 1] <- InitialState

PSim <- t(EigenSys$values[1] * matrix(0)) 
PSim <- matrix(c(1-ParamA, ParamA, 0, ParamA, 0, 1-ParamA, 0, 1-ParamA, ParamA), 3, 3, byrow=T)

for (T in 1:MaxTime) {
  for (C in 1:NumChains) {
    Current <- StateMatrix[C, T]
    StateMatrix[C, T + 1] <- sample(1:3, size = 1, prob = PSim[Current, ])
  }
}


FractionInState1 <- colMeans(StateMatrix == 1)


PlotTime <- 0:MaxTime
plot(PlotTime, FractionInState1, type = "l", col = "blue", lwd = 2,
     main = paste("Convergence to Stationary (N =", NumChains, ")"),
     xlab = "Time (n)", ylab = "Fraction in State 1")
