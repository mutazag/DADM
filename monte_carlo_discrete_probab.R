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


duration_serial = T1 + T2
unique_duration = unique(duration_serial)
for (d in unique_duration){
  print(paste(d, sum(duration_serial==d) / length(duration_serial)))
}
hist(duration_serial, ylim = c(0,length(duration_serial)), xlim=c(0,10))


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
# https://www.youtube.com/watch?v=GduPrU0vIWE