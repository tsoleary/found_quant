# Wright-Fisher model with genetic drift and mutation --------------------------
# Created by Easton R. White
# Last edited: 19-Feb-2019

# model function -------
sample_population <- function(p, N, u = 0){
  next_pop <- vector('character', length = N)
  for (i in 1:N){
    new_allele = sample(x = c('A','a'), size = 1, prob = c(p, 1 - p))
      if (u > 0){
        if (new_allele == 'a' & rbinom(1, 1,prob = u)){
        new_allele = 'A'
        }
      }
    next_pop[i] <- as.character(new_allele)
  }
  return(sum(next_pop == 'A') / N) # returns the value for p
}

# initial plotting of the freq(A) over time ------------------------------------

# Setting up parameter values
N <- 100           # population size 
p_ini <- 0.1       # the initial frequency of the A allele
max.time <- 100    # time to run simulations

p = vector(mode = 'numeric',length = max.time)
p[1] = p_ini

for (t in 1:(max.time - 1)){
  p[t+1] = sample_population(p[t], N)
}

plot(1:max.time, p, ylim = c(0, 1), type = 'l', las = 1, 
     ylab = 'Freq(A)', xlab = 'Time')

# Questions --------------------------------------------------------------------

# 1. What is the effect of population size (N) on the probability of -----------
# extinction of A allele? 

# Setting up parameter values
N <- 100           # population size 
p_ini <- 0.1       # the initial frequency of the A allele
max.time <- 100    # time to run simulations

pop_size <- c(100, 250, 500, 750, 1000, 1250, 1500, 1750, 2000)
num_reps <- 20

prop_zero <- vector(mode = 'numeric', length = length(pop_size))

for (pop in 1:length(pop_size)) {

  for (rep in 1:num_reps){
    
    p = vector(mode = 'numeric', length = max.time)
    p[1] = p_ini
    
      for (t in 1:(max.time - 1)){
        p[t+1] <- sample_population(p[t], pop_size[pop])
      }
    
    if (p[100] == 0) {
      prop_zero[pop] <- prop_zero[pop] + 1
    }
    
  }
 
}

prop_extinct <- prop_zero / num_reps

plot(pop_size, prop_extinct, xlim = c(100, 2000), ylim = c(0, 1),
     ylab = "Probability of extinction of 'A' allele", xlab = "Population Size",
     main = "Effect of population size on extinction rate")


# 2. How does the initial frequency of the A allele affect the probability -----
# it will reach fixation?

# Setting up parameter values
N <- 100           # population size 
max.time <- 100    # time to run simulations

p_init_a <- seq(0.1, 0.9, by = 0.1)
num_reps <- 20

prop_zero <- vector(mode = 'numeric', length = length(p_init_a))

for (i in 1:length(p_init_a)) {
  
  for (rep in 1:num_reps){
    
    p = vector(mode = 'numeric', length = max.time)
    p[1] = p_init_a[i]
    
    for (t in 1:(max.time - 1)){
      p[t+1] <- sample_population(p[t], N)
    }
    
    if (p[100] == 0) {
      prop_zero[i] <- prop_zero[i] + 1
    }
    
  }
  
}

prop_extinct <- prop_zero / num_reps

plot(p_init_a, prop_extinct, xlim = c(0, 1), ylim = c(0, 1),
     ylab = "Probability of extinction of 'A' allele", xlab = "Initial freq(A)",
     main = "Effect of initial 'A' allele frequency on extinction rate")


# 3. Let's now study the combined effects of mutation and genetic drift. 
# Modify the above code to include a probability, u, that if a small "a" allele 
# is chosen, it will mutate to be a big A allele. How does this affect our 
# findings in questions 1 and 2?




