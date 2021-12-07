####################         .
#                         __/ \__
#                         \     /
#                         /.'o'.\
# christmas                .o.'.
# tree                    .'.'o'.
# from                   o'.o.'.o.
# Barnsley              .'.o.'.'.o.
# Fern                 .o.'.o.'.o.'.
###################       [_____]

library(grid)

A=vector("list", 4)
# shrinking in x direction
A[[1]] = matrix(c(.03,0,
                  0,0.1), nrow=2)
#shrinking
A[[2]] = matrix(c(0.86, 0.0, 
                  0.0, 0.86), nrow=2)
#rotation to the right+shrinkage
A[[3]] = matrix(c(0.2, 0.2,
                  -0.28, 0.28), nrow=2)
#rotation to the left
A[[4]] = matrix(c(-0.15, 0.36,
                  0.28, 0.245), nrow=2)

#translation in y -direction
b=vector("list",4)
b[[1]] = matrix(c(0,2))
b[[2]] = matrix(c(0,2.5))
b[[3]] = matrix(c(0.00,0.6))
b[[4]] = matrix(c(0,0.5))

prob=c(.02, .9, .15, .12)
#prob=c(1, 1, 1, 1)

n<- 50000
x <- numeric(n)
y <- numeric(n)
x[1] <- y[1]<- 0

############
for (i in 1:(n-1)){
  trans <- sample(1:4, prob=prob, size=1)
  xy <- A[[trans]]%*%c(x[i], y[i]) + b[[trans]]
  x[i+1] <- xy[1]
  y[i+1] <- xy[2]
}

plot(x, y, col='darkgreen', cex=0.1)

###############################
#
#   Koch Snowflake
#
##############################
library(alphahull)
plot(koch( side=2, niter=5), cex=0.6, pch=11)
plot(rkoch(n=70000, side=2, niter=5), cex=0.6, pch=11)
