rm(list=ls())

# checking if readxl package is installed or not. if not installing it
if(!require("readxl", character.only = TRUE)) {
  install.packages("readxl")
  install.packages("ggplot2")
}

# loading readlxl library
library(readxl)
library(ggplot2)

# Inverse triangle function definition
inv_triangle_cdf <- function(P, vmin, vml, vmax){
  
  Pvml <- (vml-vmin)/(vmax-vmin)
  
  return(ifelse(P < Pvml,
                vmin + sqrt(P*(vml-vmin)*(vmax-vmin)),
                vmax - sqrt((1-P)*(vmax-vml)*(vmax-vmin))))
}

# no of simulation trials
n=10000

# co-founder fixed salary
cost_cofounder <-  50000

# cloud fixed cost
cloud_service <-  4264

# reading scenarios names (sheet names) from excel sheet
scenarios_names <- excel_sheets(path = "cost_estimation.xlsx")

# creating a excel sheet
cost_estimatons = list()

# reading all sheets in excel file and creating a dictionary
for(x in scenarios_names) {
  cost_estimatons[[x]] <- read_excel("cost_estimation.xlsx", sheet = x)
}

# different stage dictionary
#stages <- list(a="R&D", b="Trials", c="Production and Sales")

# R & D scenarios dictionary
r_and_d_scenario <- list("1"="in_house_cofounders","2"="in_house_external","3"="sub_contract")

# data frame for yearly stages, a = R & D, b = Trials, c = Production and Development
yearly_stage = c('a','a','b','b','c')

# configure R & D quartlery scenario based on number of year(4 quarter) R&D stage runs
qtr_res_dev_sceneario <- c(1,1,1,2,2,3,3,3)

# cost simulation
cost_simulation <- function(cost_est) {
  
  #set seed for reproducibility
  set.seed(42)
  
  #create data frame with rows = number of trials and cols = number of tasks
  csim <- as.data.frame(matrix(nrow=n,ncol=nrow(cost_est)))
  
  # for each task
  for (i in 1:nrow(cost_est)){
    #set task costs
    vmin <- cost_est$Minimum[i]
    vml <- cost_est$`Most Likely`[i]
    vmax <- cost_est$Maximum[i]
    
    #generate n random numbers (one per trial)
    psim <- runif(n)
    #simulate n instances of task
    csim[,i] <- inv_triangle_cdf(psim,vmin,vml,vmax) 
  }
  
  ctot <-  cost_cofounder + cloud_service
  #sum costs for each trial
  for(i in 1:ncol(csim)) {
    ctot <- ctot + csim[,i]
  }
  
  return(ctot)

}

# data frame to store quarterly minimum, most likely and minimum cost estimation
quarterly_estimation <<- data.frame(Quarter=numeric(), Minimum=numeric(),`Most Likely`=numeric(), Maximum=numeric())

# global variable for quarterly research and development count
qtr_dev_count <<- 1

# global variable for quarterly count
qtr_count <<- 1

# function to create data frame for quarterly cost simulation based on scenarios
qtr_sim_df <- function(cost_tot) {
  # costs corresponding to 5, 50 and 95% chance
  quan <- quantile(ecdf(cost_tot),c(0.05,0.5,0.95),type=7)
  # creating a data frame 
  df = data.frame('Quarter'=qtr_count,"Minimum"=quan[[1]],"Most Likely"=quan[[2]],"Maximum"=quan[[3]])
  # adding row into a quarterly data frame
  quarterly_estimation <<- rbind(quarterly_estimation, df)
  # increasing overall quarterly count
  qtr_count <<- qtr_count + 1
}

# function to calculate cost estimation for research and development quarterly
researchdev_cost <- function() {
 # quaterly run
   for(i in 1:4) {
    # choosing quarterly scenario
    qtr_scenario <- qtr_res_dev_sceneario[qtr_dev_count]
    # selecting quarterly estimation based on choosen scenario
    qtr_cost_est <- cost_estimatons[[r_and_d_scenario[[qtr_scenario]]]]
    # quarterly total cost simulation
    qtr_ctot <- cost_simulation(qtr_cost_est)
    # calling function to create a quaterly dataframe corresponding to 5,50,95
    qtr_sim_df(qtr_ctot)
    # increasing count of quarterly research and development
    qtr_dev_count <<- qtr_dev_count + 1
  }  
}

# function to calcualte cost estimation for trials
trial_cost <- function() {
  for(i in 1:4) {
    trial_qtr_ctot <- cost_simulation(cost_estimatons[["trial_period"]])
    qtr_sim_df(trial_qtr_ctot) 
  }
}

# function to calculate cost estimation for production and sales
prod_sales_cost <- function() {
  #prod_qtr_ctot <- cost_simulation(cost_estimatons[['prod_sales']])
  #qtr_sim_df(prod_qtr_ctot)
}

# running a loop for different stages in 5 year
for(stage in yearly_stage) {
    # call function based on yearly stage (e.g. R&D, Trial, Production and Sales)
    switch(stage, "a" = researchdev_cost(), "b" = trial_cost(), "c" = prod_sales_cost())
}

# Print quarterly estimation for scenarios
print(quarterly_estimation)

plot_graph <- function(data, xaxis, xaxis_name) {
  # Plot graph
  ggplot(data, aes(x=xaxis)) +
    geom_line(aes(y=Minimum), color = "darkred",linetype="longdash") +
    geom_line(aes(y=`Most.Likely`), color = "steelblue") +
    geom_line(aes(y=Maximum), color = "darkgreen",linetype="twodash") +
    xlab(xaxis_name) +
    ylab("Costs") +
    ggtitle("Costs corresponding to 5, 50 and 95% chance for R&D, Trials and Production & Sales")
}

#  data frame to store yearly cost by adding quarterly cost
yearly_cost_df <<- data.frame()

# function to calculate yearly cost by adding quarterly cost in particular year
yearly_cost <- function(qtrly_est) {
  count <- 1
  min_cost <- 0
  ml_cost <- 0
  max_cost <- 0
  year = 1
  for(i in 1:nrow(qtrly_est)) {
    min_cost <- min_cost + qtrly_est$Minimum[i]
    ml_cost <- ml_cost + qtrly_est$`Most.Likely`[i]
    max_cost <- max_cost + qtrly_est$Maximum[i]
    if(count == 4) {
      yr_df <- data.frame('Year'=year,'Minimum'=min_cost,'Most Likely'= ml_cost,"Maximum"=max_cost)
      yearly_cost_df <<- rbind(yearly_cost_df, yr_df)
      count <- 0
      min_cost <- 0
      ml_cost <- 0
      max_cost <- 0
      year = year + 1
    }
    count <- count + 1
  }
  return(yearly_cost_df)
}

# Calculate yearly cost
yearly_estimation <- yearly_cost(quarterly_estimation)
yearly_estimation

# Plot quarterly
plot_graph(quarterly_estimation, quarterly_estimation[[1]], colnames(quarterly_estimation)[1])

# Plot yearly
plot_graph(yearly_estimation, yearly_estimation[[1]], colnames(yearly_estimation)[1])
