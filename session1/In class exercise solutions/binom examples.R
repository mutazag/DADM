success <- 0:30

plot(success, dbinom(success, size=30, prob=.5),type='h',ylab="prob")


##prob of exactly 10 heads
dbinom(10,30,0.5)

##prob of between 10 and 15 heads
pbinom(15,30,0.5)-pbinom(9,30,0.5)

plot(success, pbinom(success,size=30,prob=0.5),type='l',ylab="cumulative prob")
     