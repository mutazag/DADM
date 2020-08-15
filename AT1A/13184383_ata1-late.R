# read tasks duration file
df_tasks = read.csv('tasks_durations.csv')
print(df_tasks)


# shift most likely estaimtes for early tasks half way closer to v3
df_tasks[1:3,]$v2 = (df_tasks[1:3,]$v3 + df_tasks[1:3,]$v2)/2


# uniform random sampling for n runs
n = 10000

#### Inverse Sampling Transform ####

# Sampling Inverse transformation sampling takes
# uniform samples of a number r between 0 and 1, interpreted as
# a probability, and then returns the largest number x from the
# domain of the distribution

# inverse sampling functions for triangular and discrete distribution

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
  # distribution, PMF. suitable for simulating discrete estimates
  
  cdf=cumsum(pmf)

  x = ifelse(P <= cdf[1], v[1], v[2])
  return(x)
}

inv_discrete2_cdf <- function(n=1000, v=c(2,3), pmf=c(.75,.25)){
  # this function approaches the sampling of discrete values  differently,
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

# for each task, simulate values using the appropriate inverse sampling method
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

# Calculate total project duration, from sequence diagram
# project_duration = T1 + max(T2, T3) + T4 + T5

tsim["total_duration"] = tsim$V1 + pmax(tsim$V2, tsim$V3) + tsim$V4 + tsim$V5

#### calc of project duration using simple arithmeitc on min, ml and max values ####

d_mins = append(df_tasks[1:4,]$v1, df_tasks[5,]$v1)
d_mls = append(df_tasks[1:4,]$v2, df_tasks[5,]$v1)
d_max = append(df_tasks[1:4,]$v3, df_tasks[5,]$v2)

project_duration <- function(tasks_d){
  return(tasks_d[1] + pmax(tasks_d[2],tasks_d[3]) + tasks_d[4] + tasks_d[5])
  
}

project_min_all = project_duration(d_mins)
project_ml_all  = project_duration(d_mls)
project_max_all = project_duration(d_max)


#### analyse results ####

Fx <- ecdf(tsim$total_duration)
qq = quantile(ecdf(tsim$total_duration),c(0.05,0.5,0.9,.95,1),type=7)
Fx(project_duration(d_mins))
Fx(project_duration(d_mls))
Fx(project_duration(d_max))
Fx(27)

print(paste0("probability to finish in ", project_min_all, "d is ", Fx(project_min_all)))
print(paste0("probability to finish in ", project_ml_all, "d is ", Fx(project_ml_all)))
print(paste0("probability to finish in ", project_max_all, "d is ", Fx(project_max_all)))


print(paste0("probability to finish in ", 27, "d is ", Fx(27)))

print(paste0("90% likely completion time is ", qq["90%"], "d "))


#### plots ####



hist(tsim$total_duration, breaks=50, probability = TRUE, 
     main = "Project Duration PDF", xlab="estimated duration",
     xlim=c(19,40))
lines(density(tsim$total_duration), lwd=2)
plot(ecdf(tsim$total_duration), main = "Project Duration CDF",ylab = "likelihood", xlab="estimated duration")
x = qq['90%']
y = .9
abline(v=x, h=y, col = "lightgray")      
points(x,y,  col = "red", pch=20)
text(x=x+2,y=y, labels = paste('(',round(x,2),',',round(y,2)*100,'%)'))
x = 27
y = Fx(27)
abline(v=x, h=y, col = "lightgray")      
points(x,y,  col = "blue", pch=20)
text(x=x+2,y=y, labels = paste('(',round(x,2),',',round(y,2)*100,'%)'))
y = Fx(24)
x = 24
abline(v=x, h=y, col = "lightgray")      
points(x,y,  col = "red", pch=20)
text(x=x+2,y=y, labels = paste('(',round(x,2),',',round(y,2)*100,'%)'))



