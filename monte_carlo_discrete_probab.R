t <- c(2,3)
pmf <- c(.75, .25)

barplot(pmf, names.arg=t, ylim=c(0,1))


cdf = cumsum(pmf)


generate_estimate_Training <- function(r){

  return = 0
  if (r <= cdf[1]) { return = t[1] }
  else if (r <= cdf[2]) { return = t[2] }
  else { return = t[2] }
  
  return
  
}

## another method to generate samples from discrete distrubition 
T1_sample = sample(x=c(2,3), size=1000, prob=c(.75,.25), replace = TRUE)
Fsample = ecdf(T1_sample)
summary(Fsample)
T1_sample_x = environment(Fsample)$x
T1_sample_y = environment(Fsample)$y
plot(ecdf(T1_sample)) # correct
hist(T1_sample, xlim=c(0,5), ylim=c(0,1000))
summary(ecdf(T1_sample))
summary.stepfun(Fsample)
Fsample(0)
Fsample(1.5)
Fsample(2)
Fsample(2.1)
Fsample(2.9)
Fsample(3)
# old method
results = c()

T1 = c()
T2 = c()
for (i in 1:1000){
  results = append(results,generate_estimate_Training(runif(1)))
  T1 = append(T1, generate_estimate_Training(runif(1)))
  T2 = append(T2, generate_estimate_Training(runif(1)))
}

print( paste("2: ",  sum(results==2)/length(results)))
print( paste("3: ",  sum(results==3)/length(results)))
hist(results, ylim = c(0,length(results)), xlim=c(0,4))
plot(ecdf(results))
plot(ecdf(T1))
plot(ecdf(T2))

duration_serial = T1 + T2
unique_duration = unique(duration_serial)
for (d in unique_duration){
  print(paste(d, sum(duration_serial==d) / length(duration_serial)))
}
hist(duration_serial, ylim = c(0,length(duration_serial)), xlim=c(0,10))
plot(ecdf(duration_serial))

df = data.frame(T1, T2) 
df['serial'] = df$T1 + df$T2
df['parallel'] = apply(df[c('T1','T2')], 1, max)

unique_s_duration = unique(df$serial)
unique_p_duration = unique(df$parallel)

printresults <- function(duraitoncoln, label){
  print(label)
  unique_d = unique(duraitoncoln)
  for (d in unique_d){
    print(paste(d, sum(duraitoncoln==d) / length(duraitoncoln)))
  }
  hist(duraitoncoln, ylim = c(0,length(duraitoncoln)), xlim=c(0,10))
  
}

printresults(df$serial, 'serial')
printresults(df$parallel, 'parallel')
printresults(df$T1, 'T1')
printresults(df$T2, 'T2')

plot(ecdf(df$serial))
plot(ecdf(df$parallel))
plot(ecdf(df$parallel+df$serial))
# https://www.youtube.com/watch?v=GduPrU0vIWE


Fserial <- ecdf(df$serial)
Fparallel <- ecfg(df$parallel)
FT1 <- ecdf(df$T1)
FT2 <- ecdf(df$T2)


plot(FT1)
plot(FT2)

summary.stepfun(FT1)
