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

#read in cost data

task_costs <- read.csv(file="session1/task_costs.csv", stringsAsFactors = F)
str(task_costs)

#set seed for reproducibility
set.seed(42)

#create data frame with rows = number of trials and cols = number of tasks
csim <- as.data.frame(matrix(nrow=n,ncol=nrow(task_costs)))

# for each task
for (i in 1:nrow(task_costs)){
  #set task costs
  vmin <- task_costs$cmin[i]
  vml <- task_costs$cml[i]
  vmax <- task_costs$cmax[i]
  
  #generate n random numbers (one per trial)
  psim <- runif(n)
  #simulate n instances of task
  csim[,i] <- inv_triangle_cdf(psim,vmin,vml,vmax) 
}

#sum costs for each trial
ctot <- csim[,1] + csim[,2] + csim[,3] + csim[,4] #costs add
ctot

#cost distribution
hist(ctot)

#mean, max, min and median cost
mean(ctot)
max(ctot)
min(ctot)
median(ctot)

#standard deviation
sd(ctot)

#plot cdf
plot(ecdf(ctot))

# costs corresponding to 5, 50 and 95% chance
quantile(ecdf(ctot),c(0.05,0.5,0.95),type=7)

#can find the chance of the cost coming in under $25K by trial and error
#or inverting the ecdf
quantile(ecdf(ctot),.59,type=7)

#Exercise: estimate duration.




