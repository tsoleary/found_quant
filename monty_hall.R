# monty hall -----

# not working!!!!!!

show <- function(left){
  goats <- which(grepl("goat", left))
  if (length(goats) > 1) {
    open <- sample(goats, 1)
  } else {
    open <- goats
  }
  return(open)
}

doors <- 1:3
prize <- c("goat1", "goat2", "car")

stay <- vector(mode = "character", length = 1000)
go <- vector(mode = "character", length = 1000)

for (i in 1:1000) {
  
  pick_door <- sample(doors, 1)
  left <- prize[-pick_door]
  open_door <- show(left)
  
  switch_door <- doors[-c(pick_door, open_door)]
  
  original_pick <- prize[pick_door]
  switch_pick <- prize[switch_door]
  
  stay[i] <- original_pick
  go[i] <- switch_pick
  
}

p_car_stay <- sum(stay == "car")/length(stay)
p_car_go <- sum(go == "car")/length(go)


