A <- matrix(c(2,0,1,3),ncol = 2)
B <- matrix(c(5,2,4,-1),ncol = 2)
A+B
A-B
diag(c(4,1,2,3))
m <- diag(3,nrow =5) #Create a 5x5 with 3 across all the diagnol position
m[2:5,1] <- 2 # makes all the values on col 1 = 2, except row 1
m[1,2:5] <- 1 # makes all the values on row 1 = 1, except col 1
m