# infectious disease modeling --------------------------------------------------

library(deSolve)

# create an SIR function
sir <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}

# set parameters -----
# proportion in each compartment: susceptible, infected, recovered 
init <- c(S = 1-1e-6, I = 1e-6, R = 0.0)
# beta and gamma infection and recovery parameter
parameters <- c(beta = 2, gamma = 0.99)
# time of the model
times <- seq(0, 70, by = 1)

# solve using ode (Ordinary Differential Equations) -----
out <- ode(y = init, times = times, func = sir, parms = parameters)
# change to data frame
out <- as.data.frame(out)
# delete time variable
out$time <- NULL

# plot
matplot(x = times, y = out, type = "l",
        xlab = "Time", ylab = "Susceptible and Recovered", main = "SIR Model",
        lwd = 1, lty = 1, bty = "l", col = 2:4)
legend(40, 0.7, c("Susceptible", "Infected", "Recovered"), 
       pch = 1, col = 2:4, bty = "n")

# modeling with vaccination ----------------------------------------------------

epi203v <- function(pars){
  
  ## NOTICE that this includes a new parameter "vaccination"
  
  ## Show parameters
  print(pars) 
  
  ## Additional parameters
  times <- seq(from = 0, to = 600, by = 1)
  yinit <- c(Susc=0.9, Infected=0.1, Recovered=0)  
  
  ## SIR model with vaccination (vaccine takes people from the Susceptible and put them in the Recovered)
  SIR_model <- function(times,yinit,pars){
    
    with(as.list(c(yinit,pars)), {
      
      dSusc      <- birth - beta*Infected*Susc                     - vaccination*Susc - death*Susc
      dInfected  <-         beta*Infected*Susc - recovery*Infected                    - death*Infected
      dRecovered <-                              recovery*Infected + vaccination*Susc - death*Recovered
      
      return(list(c(dSusc, dInfected, dRecovered)))}) 
    
  }
  
# run the ode solver for the function specified (function defined above is used)
# return the value of each compartment (Susc, Infected, Recovered) for each time step.
  results <- ode(func = SIR_model,times = times,y = yinit,parms = pars)
  results <- as.data.frame(results)
  
  return(results)
}


test.pars.v <- c(beta = 0.1, recovery = 0.005, death = 0.001, 
                 birth = 0.001, vaccination = 0.1)
results.v   <- epi203v(test.pars.v)

# plot
matplot(x = results.v[,1], y = results.v[,2:4], type = "l", lty = 1)
legend("topright", col = 1:3, legend = c("S", "I", "R"), lwd = 1)

