# The drunkard's dartboard 
# see: 
#https://eight2late.wordpress.com/2011/02/25/the-drunkard%E2%80%99s-dartboard-an-intuitive-explanation-of-monte-carlo-methods/

trials <- function(n=1000){
  trials_df <- data.frame("x"=runif(n),"y"=runif(n))
  return (trials_df)
}

trials_df <- trials(n=10000)

hit <- function(x,y){
  return (ifelse((x-0.5)^2 + (y-0.5)^2 < 0.25, 1, 0))
}


hits_df <- hit(trials_df$x, trials_df$y)

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
