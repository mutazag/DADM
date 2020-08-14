#Monte carlo simulation of a project whose tasks can be described by
#triangular distributions.
#see:
#https://eight2late.wordpress.com/2018/03/27/a-gentle-introduction-to-monte-carlo-simulation-for-project-managers/

rm(list=ls())

# The simulation uses the inverse transform method, which requires knowledge
# of the inverse of the cumulative distribution. In the case of the triangular
# distribution, this inverse can be calculated exactly (see 
# https://vimeo.com/308821283. In more general cases one would have to 
# use numerical inversion techniques.

inv_triangle_cdf <- function(P, vmin, vml, vmax){

  Pvml <- (vml-vmin)/(vmax-vmin)
  
  return(ifelse(P < Pvml,
                vmin + sqrt(P*(vml-vmin)*(vmax-vmin)),
                vmax - sqrt((1-P)*(vmax-vml)*(vmax-vmin))))
}

#no of simulation trials
n=10000

#read in duration data

task_durations <- read.csv(file="./session1/task_durations.csv", stringsAsFactors = F)
str(task_durations)


#set seed for reproducibility
set.seed(42)

#create data frame with rows = number of trials and cols = number of tasks
tsim <- as.data.frame(matrix(nrow=n,ncol=nrow(task_durations)))

# for each task
for (i in 1:nrow(task_durations)){
  #set task costs
  vmin <- task_durations$tmin[i]
  vml <- task_durations$tml[i]
  vmax <- task_durations$tmax[i]
  
  #generate n random numbers (one per trial)
  psim <- runif(n)
  #simulate n instances of task
  tsim[,i] <- inv_triangle_cdf(psim,vmin,vml,vmax) 
}

#sum costs for each trial
ttot <- pmax(tsim[,1], tsim[,2]) + tsim[,3] + tsim[,4] 
ttot

#cost distribution
hist(ttot)

#mean, max, min and median cost
mean(ttot)
max(ttot)
min(ttot)
median(ttot)

#standard deviation
sd(ttot)

#plot cdf
plot(ecdf(ttot))

# costs corresponding to 5, 50 and 95% chance
quantile(ecdf(ttot),c(0.05,0.5,0.95),type=7)

#can find the chance of the cost coming in under  by trial and error
#or inverting the ecdf
quantile(ecdf(ttot),.59,type=7)

#Exercise: estimate cost.




