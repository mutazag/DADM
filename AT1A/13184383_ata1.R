# read tasks duration file
df_tasks = read.csv('tasks_durations.csv')
print(df_tasks)

# setup uniform random sampling for n runs
n = 1000
# r = runif(n, min = 0, max = 1)
# summary(r)
# hist(r, breaks = 50, freq = FALSE)
# plot(ecdf(r))



#### Inverse Sampling Transform ####

# Sampling Inverse transformation sampling takes
# uniform samples of a number r between 0 and 1, interpreted as
# a probability, and then returns the largest number x from the
# domain of the distribution

# here we will code inverse sampling functions for triangular and discrete
# distribution

inv_triangle_cdf <- function (P, vmin, vml, vmax){
  # inverse triangle cdf is used to translate probabilities based on triangular
  # PDF, stuiable for 3-point estimate simulation 
  Pvml <- (vml-vmin)/(vmax-vmin)
  
  return(ifelse(P < Pvml,
                vmin + sqrt(P*(vml-vmin)*(vmax-vmin)),
                vmax - sqrt((1-P)*(vmax-vml)*(vmax-vmin))))
}

inv_discrete_cdf <- function(P=runif(1000),  v=c(2,3), pmf=c(.75,.25)){
  # inverse discrete cdf is used to translate probabilities based on discrete
  # distribution, PMF, suitable simulating discrete estimates
  
  cdf=cumsum(pmf)

  x = ifelse(P <= cdf[1], v[1], v[2])
  return(x)
}

inv_discrete2_cdf <- function(n=1000, v=c(2,3), pmf=c(.75,.25)){
  # this function approaches the sampling of discrete values based differently,
  # it will generate n samples of values v from the discrete distribution
  # specified by pmf
  
  return (sample(x=v, size=n, prob=pmf, replace = TRUE))
  
}

test_inv_discrete_cdf_functions <-function(trials=1000, invFn = "inv_discrete_cdf"){
  # testing inv_discrete_cdf functions to compare the two methods
  samples = c()
  for (i in 1:trials){
    s = sum(get(invFn)() == 2)
    samples = append(samples, s)
  }
  samples_summary = summary(samples)
  hist(samples/trials,breaks = 50, probability = FALSE,
       main = paste0("Density of value in trials - ", invFn),
       xlab="Simulated value")
  abline(v=samples_summary["Median"]/trials, lty = 2, col = "blue")
  title(sub = paste("Median (in blue) ", samples_summary["Median"]/trials))
  
  sd(samples)
  mean(samples)
  summary(samples)
}

# test_inv_discrete_cdf_functions(invFn = "inv_discrete_cdf")
# test_inv_discrete_cdf_functions(invFn = "inv_discrete2_cdf")


#### run the simulation ####



#create data frame with rows = number of trials and cols = number of tasks
tsim <- as.data.frame(matrix(nrow=n,ncol=nrow(df_tasks)))

# for each task
for (i in 1:nrow(df_tasks)){
  
  s = sprintf("i: %s, Task: %s, estimate type = %s", i, 
              df_tasks[i,"task"], 
              df_tasks[i, ]$type)
  print(s)
  if (df_tasks[i,"type"]=="3-point"){
    tmin = df_tasks[i,'v1']
    tml = df_tasks[i,'v2']
    tmax = df_tasks[i,'v3']
    
    print(sprintf("tmin: %s, tml: %s, tmax: %s", tmin,tml,tmax))
    estimated_t = inv_triangle_cdf(
      P=runif(n), 
      vmin=tmin, 
      vml=tml, 
      vmax=tmax)
    
    tsim[,i] = estimated_t
  } else {
    t1 = df_tasks[i,'v1']
    t2 = df_tasks[i,'v2']
    p1 = df_tasks[i,'p1']
    p2 = df_tasks[i,'p2']
    print(sprintf("tmin: %s (%s), tmax: %s (%s)", t1,p1, t2,p2))
    
    estimated_t = inv_discrete_cdf(P = runif(n), v=c(t1,t2), pmf=c(p1,p2))
    tsim[,i] = estimated_t
  }
    
}

# replace v5 with discrete2 sampling function 
tsim$V6 = inv_discrete2_cdf(n)



  # print(df_tasks[i]$task)
  # #set task costs
  # vmin <- task_durations$tmin[i]
  # vml <- task_durations$tml[i]
  # vmax <- task_durations$tmax[i]
  # 
  # #generate n random numbers (one per trial)
  # psim <- runif(n)
  # #simulate n instances of task
  # tsim[,i] <- inv_triangle_cdf(psim,vmin,vml,vmax) 

hist(tsim$V1, breaks=50, probability = TRUE)
lines(density(tsim$V1))
plot(ecdf(tsim$V1))


hist(tsim$V5, probability = TRUE, xlim=c(0,4))
plot(ecdf(tsim$V5))
summary(ecdf(tsim$V5))
sum(tsim$V5==2)
hist(tsim$V6, probability = TRUE, xlim=c(0,4))
plot(ecdf(tsim$V6))
summary(ecdf(tsim$V6))
sum(tsim$V6==2)


plot(ecdf(tsim$V5), col="red")
lines(ecdf(tsim$V6))



# Calculate total project duration, from sequence diagram
# project_duration = T1 + max(T2, T3) + T4 + T5

tsim["total_duration"] = tsim$V1 + pmax(tsim$V2, tsim$V3) + tsim$V4 + tsim$V5
tsim["total_duration2"] = tsim$V1 + pmax(tsim$V2, tsim$V3) + tsim$V4 + tsim$V6

hist(tsim$total_duration, breaks=50, probability = TRUE)
lines(density(tsim$total_duration))
plot(ecdf(tsim$total_duration))

hist(tsim$total_duration2, breaks=50, probability = TRUE)
lines(density(tsim$total_duration2))
plot(ecdf(tsim$total_duration2), xlim=c(0,40))
lines(ecdf(tsim$total_duration), col="red")
lines(ecdf(tsim$V5))


#### driect calc of project duration based on min, ml and max ####

d_mins = append(df_tasks[1:4,]$v1, df_tasks[5,]$v1)
d_mls = append(df_tasks[1:4,]$v2, df_tasks[5,]$v1)
d_max = append(df_tasks[1:4,]$v3, df_tasks[5,]$v2)

project_duration <- function(tasks_d){
  return(tasks_d[1] + pmax(tasks_d[2],tasks_d[3]) + tasks_d[4] + tasks_d[5])
  
}

project_duration(d_mins)
project_duration(d_mls)
project_duration(d_max)


#### analyse results ####


