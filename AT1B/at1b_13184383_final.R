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


#### year 1:5 ####
# what if can be used to simulate one claim 
whatif <- function(year_court = 5, 
                   exploration_plan = data.frame(year = c(1,2,3,4,5),
                                                 teams = c(1,1,1,1,1)))
  {
  # loading params
  
  n_trials = get_param('simulation', 'n_trials')
  cost_of_sales <- get_param('new_claim','cost_sales')

  
  sims <- list() 
  sims[['budget']] <- append(s[['budget']], sim('budget', 'budget',n_trials,TRUE))
  
  # simulate price

  s <- list()
  print('simulating gold price')  
  for (yearn in 1:5){
    yearn_label = paste0('year',yearn)
    yearprev_label = paste0('year', yearn -1 )
    
    s[['price']][[yearn_label]] <- ifelse(yearn >1, s$price$year1, 0) + 
      sim('price_change',yearn_label,n_trials,(yearn==1)) 
    print(paste0('   year: ', yearn, ', price(90%) ', round(quantile(ecdf(s$price[[yearn_label]]), c(.9))[[1]],2)))
  }
  sims <- append(sims, s)
  
  ## for existing claim 
  # for each year: 
  #   simulate output 
  #   calculate potential proceeds (with ops cost)
  #   simulate legal and update proceeds
  s <- list() ## needs price from sims 
  for (yearn in 1:5){
    yearn_label = paste0('year',yearn)
    yearprev_label = paste0('year', yearn -1 )
    
    print(paste0('simulating year ', yearn))

    if (yearn == 1){ 
      s[['output']][[yearn_label]] <- sim('new_claim', 'predicted', n_trials)
    } else{
      s[['output']][[yearn_label]] <- s$output[[yearprev_label]] * (1 + sim('new_claim', 'change_yoy', n_trials))
    }
    print(paste0("   output: ", round(quantile(ecdf(s$output[[yearn_label]]), c(.9))[[1]],2)))
    
    s[['cost_ops']][[yearn_label]] <- sim('new_claim', 'cost_ops', n_trials)
    print(paste0("   cost of ops: ", round(quantile(ecdf(s$cost_ops[[yearn_label]]), c(.9))[[1]],2)))
    
    # calculate year end position output, price and sales and ops cost
    s[['potential_proceeds']][[yearn_label]] <- (s$output[[yearn_label]] * sims$price[[yearn_label]] * (1-cost_of_sales)) - s$cost_ops[[yearn_label]]
    print(paste0("   potential proceeds: ", round(quantile(ecdf(s$potential_proceeds[[yearn_label]]), c(.9))[[1]],2)))
  }
  sims <- append(sims, s)
  
  
  
  # simulate legal outcome 
  s <-list()
  for(yearn in 1:5){
    yearn_label = paste0('year',yearn)
    yearprev_label = paste0('year', yearn -1 )
    
    if (yearn <= year_court){
      s[['legal_cost']][[yearn_label]] <- sim('legal', 'cost_annual', n_trials )
    } else {
      s[['legal_cost']][[yearn_label]] <- 0
    }
    print(paste0("   legal cost: ", round(quantile(ecdf(s$legal_cost[[yearn_label]]), c(.9))[[1]],2)))
    
    # what is the outcome of court
    if (yearn == year_court){
      s[['legal_resolve']] <- sim('legal', 'resolve_in_favor', n_trials)
      print(paste0("   >>>> case contested (w): ", sum(s$legal_resolve)))
    }
  }
  sims <- append(sims, s)
    
  # update claim proceeds
  # need sims for potential prceeds, legal cos and legal resolve 
  s <- list()
  for (yearn in 1:5) {
    yearn_label = paste0('year',yearn)
    yearprev_label = paste0('year', yearn -1 )
    
    # update claim proceeds based with legal costs and based on court outcome
    if (yearn <= year_court){ 
      # keep all proceeds before court year less legal cost 
      s[['claim_proceeds']][[yearn_label]] <- sims$potential_proceeds[[yearn_label]] - sims$legal_cost[[yearn_label]]
    } else { 
      # keep proceeds if case resolved in our favor 
      s[['claim_proceeds']][[yearn_label]] <- sims$legal_resolve * sims$potential_proceeds[[yearn_label]]
    }
    print(paste0("   actual claim proceeds: ", round(quantile(ecdf(s$claim_proceeds[[yearn_label]]), c(.9))[[1]],2)))
   
    # Exploration outcomes 
  }
  sims <- append(sims, s)
  
  
  s <- list()
  ## simulate exploration
  for (yearn in 1:5){
    yearn_label = paste0('year',yearn)
    yearprev_label = paste0('year', yearn -1 )
    
    # exploration plan provides number of teams for each year 
    exploration <- exploration_plan %>% filter(year == yearn)
    
    #  if () yf * (yn > yf) => get revenue) { }
    # potential proceed = gold_price * gold_output * (1-sales cost)
    # assuming gold_output follows same output simulation as first min
    s[['exploration']][['potential_proceeds']][[yearn_label]] <- sims$output[[yearn_label]] * sims$price[[yearn_label]]  * (1-cost_of_sales)
    
    # found gold
    s[['exploration']][['found_gold']][[yearn_label]] <- rbinom(n=n_trials, size=1, prob = min(1,exploration$teams * get_param('future_claim', 'chance')))
    # exploration cost is (n-1) x partner cost, where n is number of teams
    # (includes internal team)
    s[['exploration']][['cost']][[yearn_label]] <- max(0,exploration$teams - 1) * get_param('future_claim', 'cost_partner')
    
  }
  
  # calculate year of discovery from simulation 
  s$exploration[['discovery_year']] <- (as.data.frame(s$exploration$found_gold) %>% rowwise() %>% 
    mutate(discovery_year = first(which(c_across(year1:year5)==1))) %>% 
    mutate(discovery_year = replace_na(discovery_year, 0)) %>% select(discovery_year))[[1]]
  
  # simulate new exploration proceeds
  for (yearn in 1:5){
    yearn_label = paste0('year',yearn)
    yearprev_label = paste0('year', yearn -1 )
    
    # yearn is after year of discovery to start making proceeds and year
    # discovery is not zero to elimiate the no discover case
    sum(s$exploration$discovery_year&(yearn > s$exploration$discovery_year))
    
  }
  
  return(s)
}


#### testing ####

exp_plan <- data.frame( year = c(1,2,3,4,5),
                        teams = c(0,1,2,3,4))

scenario1 <- whatif(year_court = 1, exploration_plan = exp_plan)

# plot(ecdf(s$output$year2))
# fx <- (ecdf(s$output$year2))
# quantile(fx, c(.5))
# bind_cols(s$output)


logging <- list()

for (year_court in 1:5){
  s <- whatif(year_court = year_court)
  
  totals <- list() 
  
  totals[['actual']] <- (data.frame(s$claim_proceeds) %>%  rowwise() %>% mutate(total = sum(c_across(year1:year5))) %>% select(total))[[1]]
  totals[['potential']] <- (data.frame(s$potential_proceeds) %>%  rowwise() %>% mutate(total = sum(c_across(year1:year5))) %>% select(total))[[1]]
  # plot(ecdf(totals$actual))
  # plot(ecdf(totals$potential))
  
  potential = quantile(ecdf(totals$potential), c(.9), type=7)
  actual = quantile(ecdf(totals$actual), c(.9), type=7)
  
  logging[[year_court]] <- paste0('court year ', year_court, ' potential: ', potential, ' actual: ', actual)
  
}


print(logging)

