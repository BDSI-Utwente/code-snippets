

library(grid)

A=vector("list", 4)
# shrink in x direction
A[[1]] = matrix(c(.03,0,
                  0,0.1), nrow=2)
# shrink by 0.86
A[[2]] = matrix(c(0.86, 0.0,
                  0.0, 0.86), nrow=2)
#rotate and shrink
A[[3]] = matrix(c(0.2, 0.2, 
                  -0.28, 0.28), nrow=2)
#rotate and shrink
A[[4]] = matrix(c(-0.15, 0.36, 
                  0.28, 0.245), nrow=2)

#translate in y direction
b=vector("list",4)
b[[1]] = matrix(c(0, 1))
b[[2]] = matrix(c(1,0))
b[[3]] = matrix(c(1, 1))
b[[4]] = matrix(c(-1, 1))


prob=c(.02, .9, .15, .12)

n<- 50000
x <- numeric(n)
y <- numeric(n)
x[1] <- y[1]<- 1

############
for (i in 1:(n-1)){
  trans <- sample(1:4, prob=prob, size=1)
  xy <- A[[trans]]%*%c(x[i], y[i]) #+ b[[trans]]
  x[i+1] <- xy[1]
  y[i+1] <- xy[2]
}

plot(x, y, col='darkgreen', cex=0.1)
