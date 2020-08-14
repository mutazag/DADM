library(lubridate)
#go to https://finance.yahoo.com/quote/AMZN/history
#set time period to max
#download entire stock history  for apple as a csv file
df <- read.csv("./session1/AMZN.csv")
df
str(df)
df$Date

df[,2:ncol(df)] <- lapply(df[,2:ncol(df)], as.numeric)
which(is.na(df$Open)|is.na(df$Close))
##df <- df[-which(is.na(df$Open)|is.na(df$Close)),]
str(df)

df$diff <- df$Close-df$Open
df$diff
df[,c("Open","Close","diff")]

#add column indicating direction of change, U (up), D (down) or N (no change)
df$Change <- ifelse(sign(df$diff)==1,"U",ifelse(sign(df$diff)==-1,"D","N"))

table(df$Change)

#calculate proportion of up and down days (probability)
days_u <- sum(df$Change=="U")
days_d <- sum(df$Change=="D")
p_u=days_u/(days_u+days_d)
p_d=days_d/(days_u+days_d)

p_u
p_d

#vector containing direction of change of stock price
Change <- as.vector(df$Change)

#function to find runs of length k in a vector x.
findruns <- function(x,k,o="U"){
  n <- length(x)
  runs <- NULL
  for (i in 1:(n-k+1)){
    if (all(x[i:(i+k-1)]==o)) runs <- c(runs,i)
  }
  return(runs)
}

#probability of a down day given that the previous day was down
length(findruns(Change,2,"D"))
p_d_d <- length(findruns(Change,2,"D"))/days_d
p_d_d

#probability of an up day given that the previous day was up
length(findruns(Change,2,"U"))
p_u_u <- length(findruns(Change,2,"U"))/days_u
p_u_u

#Exercise 1: repeat for other Big 4 tech companies: FB, GOOG, AAPL
#Exercise 2: look for long runs. Do they correlate with significant events 
#(for the company)?

