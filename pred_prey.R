# September 16, 2019
# Quantitative reasoning, species interaction class
# predator-prey exercise
# Modified from Easton White and apeescape in R bloggers


library(deSolve) # to solve a system of differential equations

# predator = P
# victim (prey) = V

# create a function that computes the derivatives in the ODE system at a given point in time

LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dV = V*(r - alpha*P)
    dP = P*(beta*V - q)
    return(list(c(dV, dP)))
  })
}

# initial parameters

Pars <- c(r = 2, alpha = 3.5, beta = .2, q = .6)
State <- c(V = 10, P = 10)
Time <- seq(0, 100, by = 1)

# convert ode() output into a dataframe and plot the system over time
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, 
                         times = Time))

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("prey", "predator"), lty = c(1,2), col = c(1,2), 
       box.lwd = 0)

# small exercise - variation of the functional response 

# 1) increase the capture efficiency in small intervals to simulate an increase in feeding time (i.e. less prey, more time searching for them).

for (a in seq(0.2, 3, by = 0.1)){
  
  Pars <- c(r = 2, alpha = a, beta = .2, q = .6)
  
  out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, 
                           times = Time))
  
  g <- matplot(out[,-1], type = "l", xlab = "time", ylab = "population", 
               main = paste("Predator & Prey Pop at alpha =", a))
  
  legend("topright", c("prey", "predator"), lty = c(1, 2), col = c(1, 2), 
         box.lwd = 0)
  
  print(g)

}




# What happens to the long-term behavior of the predator population?

