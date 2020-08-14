# The drunkard's dartboard 
# see: 
#https://eight2late.wordpress.com/2011/02/25/the-drunkard%E2%80%99s-dartboard-an-intuitive-explanation-of-monte-carlo-methods/

#write a function that returns a dataframe of randomly generated coordinates in
#the square region 0 <= x <=1 and 0 <= y <= 1. The function should take as input a positive
#integer n, representing the number of trials to be generated (default 1000)

trials <- function(n=1000){
  trials_df <- data.frame("x"=runif(n),"y"=runif(1000))
  return (trials_df)
}

#generate 10000 coordinates 
trials_df <- trials(n=10000)

#write a function that takes as input a coordinate (x,y)  and returns 1 or 0 depending on
#whether the coordinate lies inside or outside a circle with centre at (0.5,0.5) and radius 0.5.

hit <- function(x,y){
  return (ifelse((x-0.5)^2 + (y-0.5)^2 < 0.25, 1, 0))
}


hits_df <- hit(trials_df$x, trials_df$y)

#Given the above, what's your estimated area of the circle?

estimated_area <- sum(hits_df)/length(hits_df)

estimated_area

library(ggplot2)

# plot hits
p <- ggplot() + 
  geom_point(data=trials_df,aes(x,y),color="blue") +
  coord_fixed(ratio = 1)
p

# plot dartboard

circle <- function(centre = c(0.5,0.5),radius = 0.5, npoints = 100){
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- centre[1] + radius * cos(tt)
  yy <- centre[2] + radius * sin(tt)
  return(data.frame(x = xx, y = yy))
}

dartboard <- circle()

p <- p + geom_path(dartboard,mapping=aes(x=x,y=y), color="red", size=2)
p
