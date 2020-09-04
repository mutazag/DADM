# AT1B: Digging for Gold
# Mutaz Abu Ghazaleh
# 13184383

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


get_param <- function(category='budget', parameter='budget', param_file = './parameters.csv'){
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


print_quantiles <- function(label, series){ 
    fx <- ecdf(series)
    
    qq <- round(quantile(fx, c(.05,.5,.95), type=7)/1e+6, 2)
    
    print(paste0('   ', label, ' in $mil 95% over: ', qq[[1]], ', 50% over: ', qq[[2]], ', 5% over: ', qq[[3]]))
}


#### What if function ####
# what if can be used to simulate one claim
whatif <- function(year_court = 2,
                   exploration_plan = data.frame(year = c(1,2,3,4,5),
                                                 teams = c(1,2,2,2,2)))
  {
  # loading params
  n_trials = get_param('simulation', 'n_trials')
  cost_of_sales <- get_param('new_claim','cost_sales')


  sims <- list()
  sims[['budget']] <- sim('budget', 'budget',n_trials,TRUE)

  # simulate price

  s <- list()
  print('simulating gold price')
  for (yearn in 1:5){
    yearn_label = paste0('year',yearn)
    yearprev_label = paste0('year', yearn -1 )

    s[['price']][[yearn_label]] <- ifelse(rep(yearn >1, n_trials), s$price$year1, 0) +
      sim('price_change',yearn_label,n_trials,(yearn==1))
    print_quantiles(paste0('gold price ', yearn_label), s$price[[yearn_label]])
    # print(paste0('   year: ', yearn, ', price(90%) ', round(quantile(ecdf(s$price[[yearn_label]]), c(.9))[[1]],2)))
  }
  sims <- append(sims, s)

  ## for existing claim
  # for each year:
  #   simulate output
  #   calculate potential proceeds (with ops cost)
  #   simulate legal and update proceeds
  print('simulating potential proceeds')
  s <- list() ## needs price from sims
  for (yearn in 1:5){
    yearn_label = paste0('year',yearn)
    yearprev_label = paste0('year', yearn -1 )

    if (yearn == 1){
      s[['output']][[yearn_label]] <- sim('new_claim', 'predicted', n_trials)
    } else{
      s[['output']][[yearn_label]] <- s$output[[yearprev_label]] * (1 + sim('new_claim', 'change_yoy', n_trials))
    }
    print_quantiles(paste0('output ', yearn_label), s$output[[yearn_label]])
    # print(paste0("   output: ", round(quantile(ecdf(s$output[[yearn_label]]), c(.9))[[1]],2)))

    s[['cost_ops']][[yearn_label]] <- sim('new_claim', 'cost_ops', n_trials)
    print_quantiles(paste0('cost of ops ', yearn_label), s$cost_ops[[yearn_label]])
    # print(paste0("   cost of ops: ", round(quantile(ecdf(s$cost_ops[[yearn_label]]), c(.9))[[1]],2)))

    # calculate year end position output, price and sales and ops cost
    s[['potential_proceeds']][[yearn_label]] <- (s$output[[yearn_label]] * sims$price[[yearn_label]] * (1-cost_of_sales)) - s$cost_ops[[yearn_label]]
    print_quantiles(paste0('potential proceeds ', yearn_label), s$potential_proceeds[[yearn_label]])
    #print(paste0("   potential proceeds ", yearn_label, ': ', round(quantile(ecdf(s$potential_proceeds[[yearn_label]]), c(.1))[[1]],2)))
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
    print_quantiles(paste0('legal cost ', yearn_label), s$legal_cost[[yearn_label]])
    # print(paste0("   legal cost: ", round(quantile(ecdf(s$legal_cost[[yearn_label]]), c(.9))[[1]],2)))

    # what is the outcome of court
    if (yearn == year_court){
      s[['legal_resolve']] <- sim('legal', 'resolve_in_favor', n_trials)
      print(paste0("    ", yearn_label, ">>>> case contested (w): ", sum(s$legal_resolve)))
    }
  }
  sims <- append(sims, s)

  # update claim proceeds
  # need sims for potential proceeds, legal cost and legal resolve
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
    print_quantiles(paste0('claim proceeds ', yearn_label), s$claim_proceeds[[yearn_label]])
    # print(paste0("   actual claim proceeds 90% ", yearn_label, ': ', round(quantile(ecdf(s$claim_proceeds[[yearn_label]]), c(.1))[[1]],2)))
    # print(paste0("   actual claim proceeds 50% ", yearn_label, ': ', round(quantile(ecdf(s$claim_proceeds[[yearn_label]]), c(.5))[[1]],2)))
    # Exploration outcomes
  }
  sims <- append(sims, s)


  s <- list()
  ## simulate exploration
  print(exploration_plan)
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
    print_quantiles(paste0('future claim potential proceeds ', yearn_label), s$exploration$potential_proceeds[[yearn_label]])

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
    # discovery is not zero to eliminate the no discover case
    
    # shows the probability to start earning in each year given our exploration plan
    s$exploration[['producing_prob']][[yearn_label]] <- (sum(s$exploration$discovery_year&(yearn > s$exploration$discovery_year))) / n_trials
    
    
    start_producing <- as.integer(s$exploration$discovery_year&(yearn > s$exploration$discovery_year))
    
    s$exploration[['claim_proceeds']][[yearn_label]] <- 
      (start_producing * sims$output[[yearn_label]] * sims$price[[yearn_label]] * (1 - cost_of_sales))
    s$exploration[['mining_cost']][[yearn_label]] <- (s$exploration$cost[[yearn_label]] * (yearn <= s$exploration$discovery_year))
    
    s$exploration[['claim_pnl']][[yearn_label]] <- s$exploration$claim_proceeds[[yearn_label]] - s$exploration$mining_cost[[yearn_label]]
    print_quantiles(paste0('future claim pnl ', yearn_label), s$exploration$claim_pnl[[yearn_label]])
    # print(paste0("   ", yearn_label, ' future claim pnl 90%:', round(quantile(ecdf(s$exploration$claim_pnl[[yearn_label]]), c(.1))[[1]],2)))
  }
  sims <- append(sims, s)
  
  # calculate yearly pnl -- not taking budget into the pnl yet
  s <- list()
  for (yearn in 1:5){
    yearn_label = paste0('year',yearn)
    yearprev_label = paste0('year', yearn -1 )
    s[['pnl']][[yearn_label]] <- #ifelse(yearn == 1, sims$budget,0) + 
      sims$claim_proceeds[[yearn_label]] + 
      sims$exploration$claim_pnl[[yearn_label]] 
    print_quantiles(paste0('full pnl ', yearn_label), s$pnl[[yearn_label]])
    # print(paste0("   ", yearn_label, ' pnl :', round(quantile(ecdf(s$pnl[[yearn_label]]), c(.1))[[1]],2)))
  }
  sims <- append(sims, s)
  
  # calcualate end of  year position 
  s <- list() 
  for (yearn in 1:5){
    yearn_label = paste0('year',yearn)
    yearprev_label = paste0('year', yearn -1 )
    
    s[['eoy_position']][[yearn_label]] <- 
      ifelse(rep(yearn==1, n_trials), sims$budget, s[['eoy_position']][[yearprev_label]]) + 
      sims$pnl[[yearn_label]]
    print_quantiles( paste0('eoy ', yearn_label),s$eoy_position[[yearn_label]]) 
  }
  sims <- append(sims,s)
  
  sims[['final_position']] <- (as.data.frame(sims$pnl) %>% rowwise() %>% 
    mutate(total = sum(c_across(year1:year5))) %>% select(total))[[1]]
  sims$final_position = sims$final_position + sims$budget
  print_quantiles('**** final position ', sims$final_position)
  print(paste0(' ***  final position (95%)', round(quantile(ecdf(sims$final_position), c(.05))[[1]])))

  return(sims)
}


#### visualisations ####



plot_label_quantiles <-function (s_years, ci=.5){
  
  qq <- 1-c((1-ci)/2, .5, (1+ci)/2)
  q_labels <- scales::percent(c((1-ci)/2, .5, (1+ci)/2))
  ci_label <- scales::percent(ci)
  qq_ecdf <- round(quantile(ecdf(s_years), probs = qq, type=7)/1e6,2)
  
  s  <- paste0(q_labels[[2]], ' probability to exceed $',qq_ecdf[[2]],'mil\n', 
               ci_label, ' of simulations are between ', qq_ecdf[[3]] , ' and ', 
               qq_ecdf[[1]], ' million AUD' )
  
  return(s)
}



plot_yearly_boxplots <- function(s_years, title='Yearly P&L', subtitle='', caption=''){
  
  

  
  label_dollar_custom <- scales::label_dollar(scale = 1e-6, suffix='mil')
  
  plot_df <- as.data.frame(s_years) %>% 
    pivot_longer(cols=year1:year5) %>% 
    group_by(name) %>% 
    mutate(avg = ifelse(median(value)<0, 'b', 'a'))
  
  plot_df %>% ggplot() + 
    geom_boxplot(aes(x=name, y=value, fill=avg), alpha=.5) +
    geom_hline(yintercept = 0, size=1,linetype ='dotted',  colour = "grey80") +
    scale_fill_manual(values=c( 'seagreen3', 'indianred1'))+ 
    scale_y_continuous(labels = label_dollar_custom,
                       breaks = scales::pretty_breaks(n = 10), 
                       limits = c(-5e+6, 35e+6)) +
    labs(title=title, subtitle = subtitle, caption = caption) +
    theme_light() + 
    theme(axis.title = element_blank(), 
          legend.position = "none") -> p
  
  return(p)
}





plot_yearly_lines <- function(s_years, 
                              ci= .9, 
                              title='Cumulative Earnings',
                              subtitle='', 
                              caption=''){
  
  qq <- 1-c((1-ci)/2, .5, (1+ci)/2)
  q_labels <- scales::percent(c((1-ci)/2, .5, (1+ci)/2))
  ci_label <- scales::percent(ci)
  
  
  label_dollar_custom <- scales::label_dollar(scale = 1e-6, suffix='mil')
  
  plot_df <- as.data.frame(s_years) %>% 
    summarise(across(year1:year5, quantile, qq)) %>% 
    'rownames<-' (q_labels) %>% 
    rownames_to_column('quantiles') %>% 
    pivot_longer(cols=year1:year5) 
  
  plot_df %>% filter(quantiles %in% c(q_labels[1],q_labels[3])) -> df_q
  plot_df %>% filter(quantiles %in% c(q_labels[2])) -> df_m
  
  
  df_q %>% 
    ggplot(aes(x=name, y=value, group=quantiles)) +
    geom_hline(yintercept = 0, size=1, linetype ='dotted', colour = "grey80") +
    geom_line(color='lightblue2', size=1) + 
    geom_line(data=df_m, aes(x=name, y=value), color='lightblue4', size=1) +
    scale_y_continuous(labels = label_dollar_custom,
                       breaks = scales::pretty_breaks(n = 10),
                       limits = c(-5e+6, 35e+6)) +
    labs(title=title, 
         subtitle = subtitle, 
         caption = caption) +
    theme_light() + 
    theme(axis.title = element_blank()) -> p
  
  return(p)
}

#### testing ####
tests <- function(){
  exp_plan <- data.frame( year = c(1,2,3,4,5),
                          teams = c(1,1,1,1,1))
  
  scenario1 <- whatif(year_court = 1, exploration_plan = exp_plan)
  
  # plot(ecdf(s$output$year2))
  # fx <- (ecdf(s$output$year2))
  # quantile(fx, c(.5))
  # bind_cols(s$output)
  
  
  logging <- list()
  exp_plan <- data.frame( year = c(1,2,3,4,5),
                          teams = c(2,1,1,1,1))
  
  for (year_court in 1:5){
    s <- whatif(year_court = year_court, exploration_plan = exp_plan)
    logging[[year_court]] <- print_quantiles(paste0('court year ', year_court, ' final position'), s$final_position)
  }
  
  
  print(logging)
  
  
  
  s <- whatif(year_court = 1, exploration_plan = exp_plan)
  for (yearn in 1:5){
    yearn_label <- paste0('year',yearn)
    print_quantiles(paste0('scenario whatif, eoy ', yearn_label), s$eoy_position[[yearn_label]])
  }
  print_quantiles('final position', s$final_position)

}




