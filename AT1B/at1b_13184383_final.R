# AT1B: Digging for Gold 
# Mutaz Abu Ghazaleh 
# 13184383

library(tidyverse)
library(gridExtra)

inv_triangle_cdf <- function (P, vmin, vml, vmax){
  # inverse triangle cdf is used to translate probabilities based on triangular
  # PDF, stuiable for 3-point estimate simulation 
  Pvml <- (vml-vmin)/(vmax-vmin)
  
  return(ifelse(P < Pvml,
                vmin + sqrt(P*(vml-vmin)*(vmax-vmin)),
                vmax - sqrt((1-P)*(vmax-vml)*(vmax-vmin))))
}


get_param <- function(category='budget', parameter='budget', param_file = './parameters.csv'){ 
  if (!exists('sim_params')){
    print('loading sim params')
    sp <- read.csv(param_file)
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

sim_df <- function(label = 'year1', 
                category='new_claim', 
                parameter='predicted', 
                n_trials = 10000, 
                reps = FALSE){ 
  
  v <- get_param(category, parameter)
  
  df <- data.frame(a = rep(ifelse(reps, v, 0), n_trials)) 
  colnames(df) <- c(label)
  
  if (is.data.frame(v)){
    df[label] <- inv_triangle_cdf(
      P = runif(n_trials), 
      vmin = v$vmin, 
      vml = v$vml, 
      vmax = v$vmax
    )
  } else if (is.numeric(v) && !reps){
    df[label] <- rbinom(n=n_trials, size=1, prob=v)
  }
  
  return(df)
}


sim <- function(category='new_claim',
                 parameter='predicted', 
                 n_trials = 10000, 
                 reps = FALSE){ 
  
  v <- get_param(category, parameter)
  
  ret <- rep(ifelse(reps, v, 0), n_trials)

  if (is.data.frame(v)){
    ret <- inv_triangle_cdf(
      P = runif(n_trials), 
      vmin = v$vmin, 
      vml = v$vml, 
      vmax = v$vmax
    )
  } else if (is.numeric(v) && !reps){
    ret <- rbinom(n=n_trials, size=1, prob=v)
  }
  
  return(ret)
}

#### loading params ####

n_trials = get_param('simulation', 'n_trials')
cost_of_sales <- get_param('new_claim','cost_sales')
s <- list() 

s[['budget']] <- append(s[['budget']], sim('budget', 'budget',n_trials,TRUE))

year_court <- 3

#### year 1:5 ####

for (yearn in 1:5){
  print(paste0('simulating year ', yearn))
  yearn_label = paste0('year',yearn)
  yearprev_label = paste0('year', yearn -1 )
  
  s[['price']][[yearn_label]] <- ifelse(yearn >1, s$price$year1, 0) + 
    sim('price_change',yearn_label,n_trials,(yearn==1)) 
  print(paste0('   price: ', round(quantile(ecdf(s$price[[yearn_label]]), c(.5))[[1]],2)))
  
  if (yearn == 1){ 
    s[['output']][[yearn_label]] <- sim('new_claim', 'predicted', n_trials)
  } else{
    s[['output']][[yearn_label]] <- s$output[[yearprev_label]] * (1 + sim('new_claim', 'change_yoy', n_trials))
  }
  print(paste0("   output: ", round(quantile(ecdf(s$output[[yearn_label]]), c(.5))[[1]],2)))

  s[['cost_ops']][[yearn_label]] <- sim('new_claim', 'cost_ops', n_trials)
  print(paste0("   cost of ops: ", round(quantile(ecdf(s$cost_ops[[yearn_label]]), c(.5))[[1]],2)))

  # calculate year end position output, price and sales and ops cost
  s[['potential_proceeds']][[yearn_label]] <- (s$output[[yearn_label]] * s$price[[yearn_label]] * (1-cost_of_sales)) - s$cost_ops[[yearn_label]]
  print(paste0("   potential proceeds end position: ", round(quantile(ecdf(s$potential_proceeds[[yearn_label]]), c(.5))[[1]],2)))
  
  
  if (yearn <= year_court){
    s[['legal_cost']][[yearn_label]] <- sim('legal', 'cost_annual', n_trials )
  } else {
    s[['legal_cost']][[yearn_label]] <- 0
  }
  print(paste0("   legal cost: ", round(quantile(ecdf(s$legal_cost[[yearn_label]]), c(.5))[[1]],2)))
  
  # what is the outcome of court
  if (yearn == year_court){
    s[['legal_resolve']] <- sim('legal', 'resolve_in_favor', n_trials)
    print(paste0("   >>>> case contested (w): ", sum(s$legal_resolve)))
  }
  
  
  # update claim revenue based on court outcome
  if (yearn <= year_court){ 
    # keep all proceeds before court year 
    s[['claim_proceeds']][[yearn_label]] <- s$potential_proceeds[[yearn_label]]
  } else { 
    # keep proceeds if case resolved in our favor 
    s[['claim_proceeds']][[yearn_label]] <- s$legal_resolve * s$potential_proceeds[[yearn_label]]
  }
  print(paste0("   actual claim proceeds: ", round(quantile(ecdf(s$claim_proceeds[[yearn_label]]), c(.5))[[1]],2)))
  # sim_legal_cost <- sim(yearn_label,'legal', 'cost_annual', n_trials)
  # sim_legal_resolve <- sim(yearn_label,'legal', 'resolve_in_favor', n_trials)
  # 
  # 
  # sim_future_claim_cost <- sim(yearn_label, 'future_claim','cost_partner', n_trials, TRUE)
  # sim_future_claim_found <- sim(yearn_label, 'future_claim', 'chance', n_trials)
  
  
}



# plot(ecdf(s$output$year2))
# fx <- (ecdf(s$output$year2))
# quantile(fx, c(.5))
# bind_cols(s$output)

