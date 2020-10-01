#### modeling code ####
# AT2: Business Case Development
# AI-Assisted Accelerated MRI 

# Mutaz Abu Ghazaleh: 13184383 
# Jay Radhakrishnan: 13037686
# Anand KC: 13386331
# Andrew Barbour: 12822589
# Chris Garces: 12473310


library(tidyverse)
library(gridExtra)



#### functions definitions ####

inv_triangle_cdf <- function (P, vmin, vml, vmax){
  # inverse triangle cdf is used to translate probabilities based on triangular
  # PDF, stuiable for 3-point estimate simulation
  Pvml <- (vml-vmin)/(vmax-vmin)
  
  return(ifelse(P < Pvml,
                vmin + sqrt(P*(vml-vmin)*(vmax-vmin)),
                vmax - sqrt((1-P)*(vmax-vml)*(vmax-vmin))))
}


get_param <- function(category='trials', parameter='fda_approval_time', param_file = './parameters.csv'){
  if (!exists('sim_params')){
    print('loading sim params')
    sp <- read.csv(param_file,stringsAsFactors = FALSE)
    assign('sim_params', sp, envir = .GlobalEnv)
  }
  
  p <- sim_params %>% filter(cat == category, param == parameter)
  
  switch (p$type,
          '1-point' = p <- p %>% select(vmin) %>% first(),
          'prob' = p <- p %>% select(vmin) %>% first(),
          '3-point' = p <- p %>% select(vmin, vml, vmax)
  )
  
  return(p)
}

sim <- function(category='trial',
                parameter='fda_approval_time',
                n_trials = 10000,
                reps = FALSE){
  # sim function will simulate a single variable using monte-carlo
  # it is setup to accept category and parameter name to be loaded from 
  # paramaters file
  # the simulation will use inv triangle cdf if variable has a 3 point estimate
  # else it will assume a bonomial simulation or just repeates the value for all 
  # simulation trials 
  # reps: FALSe and param is 1-point, will simulate a binomial
  # reps: TRUE and param is 1-point, just repeat the value as is
  #
  # the result is  a vector of simulated values of length = number of trials 

  
  v <- get_param(category, parameter)
  
  # initialise the simulated value to 0, unless param is a numeric (not 3-point)
  ret <- rep(ifelse(reps, v, 0), n_trials)
  
  # if param is 3-point (dataframe), run simulation with inv_triangle_cdf
  if (is.data.frame(v)){
    ret <- inv_triangle_cdf(
      P = runif(n_trials),
      vmin = v$vmin,
      vml = v$vml,
      vmax = v$vmax
    )
  } else if (is.numeric(v) && !reps){
    # else if param is numeric and not to be repeated, then simulate as a binomial
    # in this case the param is meant to be a probability 
    ret <- rbinom(n=n_trials, size=1, prob=v)
  }
  
  return(ret)
}


print_quantiles <- function(label, series){ 
  fx <- ecdf(series)
  
  qq <- round(quantile(fx, c(.05,.5,.95), type=7)/1e+6, 2)
  
  print(paste0('   ', label, ' in $mil 95% over: ', qq[[1]], ', 50% over: ', qq[[2]], ', 5% over: ', qq[[3]]))
}
#### plotting functions ####

plot_sim_values <- function(s){
  qq = quantile(ecdf(s),c(0.05,0.5,0.9,.95,1),type=7)
  Fx <- ecdf(s)
  hist(s, breaks=50, probability = TRUE, 
       main = "Histogram", xlab="estimated value")
  lines(density(s), lwd=2)
  plot(ecdf(s), main = "CDF",ylab = "likelihood", xlab="estimated value")
  x = qq['95%']
  y = .95
  abline(v=x, h=y, col = "lightgray")      
  points(x,y,  col = "red", pch=20)
  text(x=x,y=y, labels = paste('(',round(x,2),',',round(y,2)*100,'%)'))
  x = qq['50%']
  y = .5
  abline(v=x, h=y, col = "lightgray")      
  points(x,y,  col = "blue", pch=20)
  text(x=x,y=y, labels = paste('(',round(x,2),',',round(y,2)*100,'%)'))
  x = qq['5%']
  y = .05
  abline(v=x, h=y, col = "lightgray")      
  points(x,y,  col = "red", pch=20)
  text(x=x,y=y, labels = paste('(',round(x,2),',',round(y,2)*100,'%)'))
  
}




#### simulation of stages #### 

# this section includes the code for simulating the different business scenarios

sim_rnd_completion <- function(external_resources=FALSE, n_trials=10000){
  # return number of last quarter of R&D
  
  if (external_resources == TRUE){
    print('rnd with external resources')
    s <- sim(category = 'r_and_d', 'duration_with_external', n_trials = n_trials)
  } else { 
    print('internal rnd')
    s <- sim(category = 'r_and_d', 'duration_internal_only', n_trials = n_trials)

  }
  
  s <- round(s)
  
  return(s)
}

sim_trials_completion <- function(rnd_completes, n_trials=10000){
  # return number of last quarter of trials 
  s <- round(sim('trials','fda_approval_time',n_trials)) + rnd_completes
  
  return(s)
}


sim_rnd_cost <- function(qtr = 1, external_resources=FALSE, n_trials = 10000){
  # first calc internal rnd cost
  cost_cofounders <- sim(category = 'cost', parameter = 'cofounders', n_trials=n_trials,rep=TRUE)
  cost_cloud <- sim(category = 'cost', parameter = 'cloud', n_trials=n_trials, rep=TRUE)
  cost_hw <- ifelse(qtr == 1,1,0) *  sim(category = 'cost', parameter = 'hw_qtr1', n_trials=n_trials)
  cost_logistics <- sim(category = 'cost', parameter = 'logistics', n_trials=n_trials)
  
  cost_internal <- cost_cofounders + cost_cloud + cost_hw + cost_logistics
  
  # then calculate external rnd costs 
  if (external_resources==TRUE){
    cost_ml_engineer <- sim(category = 'additional_resources', parameter = 'ml_engineer', n_trials=n_trials )
    cost_sw_engineer <- sim(category = 'additional_resources', parameter = 'sw_engineer', n_trials=n_trials )
    cost_sub_contract <- sim(category = 'additional_resources', parameter = 'sub_contract', n_trials=n_trials )
    
    cost_external <- cost_ml_engineer + cost_sw_engineer
  } else {
    cost_external = rep(0, n_trials)
  }
  
  # rnd cost is internal + external (if external is used)
  s <- cost_internal + cost_external
  
  return(s)
  
}

sim_trials_cost_ongoing <- function(n_trials=10000){
  # calculate ongoing rnd cost in trials 
  # trials,ongoing_ml_eng,3-point,10000,15000,20000,AUD,external ml engineer on going cost with 1% equity
  # trials,ongoing_sw_engineer,3-point,8000,12000,18000,AUD,external sw engineer on going cost with 1% equity
  s <- sim_rnd_cost(n_trials=n_trials) + 
    sim(category = 'trials', parameter = 'ongoing_ml_eng', n_trials=n_trials ) + 
    sim(category = 'trials', parameter = 'ongoing_sw_eng', n_trials=n_trials ) + 
    sim(category = 'trials', parameter = 'cost_of_sales', n_trials=n_trials )
  return(s)
  
}

sim_trials_cost <-function(qtr, rnd_completes, trials_complete, n_trials=10000){ 
  

  # fda cost during trials
  trials_year <- if_else((qtr > rnd_completes),
                         floor((qtr - rnd_completes - 1)/4)+1,
                         rep(0, n_trials ))
  
  fda_year1 <- sim('trials','fda_inital_cost_1',n_trials=n_trials, reps = TRUE) + 
    sim('trials','fda_registration_fee_1',n_trials=n_trials, reps = TRUE)
  fda_year2 <- sim('trials','fda_inital_cost_2',n_trials=n_trials, reps = TRUE) + 
    sim('trials','fda_registration_fee_2',n_trials=n_trials, reps = TRUE)
  fda_cost <- if_else( trials_year == 0, 
                       rep(0,n_trials), 
                       if_else(trials_year == 1, 
                               fda_year1, fda_year2))
  
  # ongoing rnd cost during trials period + fda_cost for same period
  s <- if_else((qtr > rnd_completes) & (qtr <= trials_complete),
               sim_trials_cost_ongoing(n_trials=n_trials) + fda_cost,
               rep(0, n_trials ))
  
  return(s)
  }


sim_prod_and_sales <- function(trials_complete, n_trials=10000){
  # simulating prod and sales 
  # simulate yearly then quarterly to account for yoy change 
  # condition to only start prod and sales after trials completion
  
  s <- list()
  for (yearn in 1:5){
    year_label = sprintf('year%02d', yearn)
    year_prev_label = sprintf('year%02d', yearn - 1)
    cost_of_sales_increase_yoy <- get_param('prod','cost_of_sales_increase_yoy')
    
    if (yearn == 1){
      for (qtr in 1:4){
        qtr_label = sprintf('qtr%02d', qtr)
        qtr_prev_label = sprintf('qtr%02d', qtr - 1)
        
        s[['revenue']][[qtr_label]] <- if_else( 
          qtr > trials_complete,
          sim('prod', 'revenue', n_trials) * (1+sim('prod','sales_growth_yoy',n_trials)),
          rep(0, n_trials=n_trials)) 
        
        # need to add cost of rnd
        s[['cost_of_sales']][[qtr_label]] <- if_else(
          qtr > trials_complete,
          sim('prod', 'cost_of_sales', n_trials),
          rep(0, n_trials=n_trials)) 
      }
    }
    
    if (yearn > 1){
      q_start = (yearn * 4) - 3
      q_end = (yearn * 4)
      for (qtr in q_start:q_end){
        qtr_label = sprintf('qtr%02d', qtr)
        qtr_yoy_prev_label = sprintf('qtr%02d', qtr - 4)
        
        s[['revenue']][[qtr_label]] <- if_else(
          qtr <= trials_complete, rep(0, n_trials=n_trials) , 
          if_else(qtr <= trials_complete + 4, 
                  sim('prod', 'revenue', n_trials) * (1+sim('prod','sales_growth_yoy',n_trials)),
                  s[['revenue']][[qtr_yoy_prev_label]] * (1 + sim('prod','sales_growth_yoy',n_trials))))
        
        s[['cost_of_sales']][[qtr_label]] <- if_else(
          qtr <= trials_complete, rep(0, n_trials=n_trials) , 
          if_else(qtr <= trials_complete + 4, 
                  sim('prod', 'cost_of_sales', n_trials) * (1+cost_of_sales_increase_yoy),
                  s[['cost_of_sales']][[qtr_yoy_prev_label]] * (1 + cost_of_sales_increase_yoy)))
        
      }
    }
    
  }
  return(s)
}

#### scenario whatif simulation ####

whatif <- function(rnd_external = FALSE){ 
  n_trials = get_param('simulation', 'n_trials')
  
  # find when R&D will complete qtr numbers
  rnd_completes <- sim_rnd_completion(rnd_external,n_trials=n_trials)
  trials_complete <- sim_trials_completion(rnd_completes,n_trials=n_trials) 
  
  s <- list() 
  s[['rnd_completes']] <- rnd_completes
  s[['trials_complete']] <- trials_complete
  
  # calculate rnd and trials cost
  for (qtr in 1:20){ 
    
    qtr_label = sprintf('qtr%02d', qtr)
    qtr_prev_label = sprintf('qtr%02d', qtr - 1)
    
    # if before rnd_completes, this is the cost of RND
    cost_rnd <- if_else(qtr <= rnd_completes, 
            sim_rnd_cost(qtr=qtr, external_resources=rnd_external, n_trials=n_trials), 
            rep(0, n_trials=n_trials))
  
    s[['cost_rnd']][[qtr_label]] <- cost_rnd
    
    # cost of trials = ongoing costs for founds and rnd plus engineering plus FDA 
    # conditioned on qtr is in trial period  
    cost_trials <- sim_trials_cost(qtr,rnd_completes,trials_complete,n_trials)
    
    s[['cost_trials']][[qtr_label]] <- cost_trials
    
  }
  
  # prod and sales
  s[['sales']] <- sim_prod_and_sales(trials_complete)
  
  return(s)
}

  
#### testing ####

test <- function(){ 
  # 
  # plot_sim_values(sim(category = 'trials', 'fda_approval_time_days'))
  # plot_sim_values(sim(category = 'trials', 'cost_of_sales'))
  # plot_sim_values(sim(category = 'r_and_d', 'duration_internal_only'))
  # plot_sim_values(sim(category = 'r_and_d', 'duration_with_external'))
  # 
  # plot_sim_values(sim(category = 'prod', 'revenue'))
  # plot_sim_values(sim(category = 'prod', 'cost_of_sales'))
  # 
  # sim(category = 'prod', 'cost_of_sales_increase_yoy', reps = TRUE)
  # sim(category = 'prod', 'cost_of_sales_increase_yoy', reps = TRUE)
  
  scenario1 <- whatif()
}


