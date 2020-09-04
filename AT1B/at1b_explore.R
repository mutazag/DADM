
source('at1b_13184383_final.R')

#### Scenario 1: no exploration ####
scenarios <- list() 
for (year_court in 1:5){ 
  
  exploration_plan = data.frame(year = c(1,2,3,4,5),
                                teams = c(0,0,0,0,0))
  
  s <- whatif(year_court, exploration_plan)
  s[['exploration_plan']] <- exploration_plan
  s[['year_court']] <- year_court
  
  scenarios[[paste0('year_court_', year_court)]] <-s
}


for (s in scenarios){
  print(s$year_court)
  sfinalposition <- plot_label_quantiles(s$final_position)
  p1 <- plot_yearly_boxplots(s$eoy_position, 
                            title = 'End of year position for simulated scenario', 
                            subtitle = sfinalposition,
                            caption = paste0('Go to court in year ', s$year_court, ' no exploration')
                            )
  plot_file_name1 <- paste0('s1_eoy_position_box_court_',s$year_court,'_no_exploration.png')
  png(paste0('./img/',plot_file_name1))
  print(p1)
  dev.off()
  p2 <- plot_yearly_lines(s$eoy_position, 
                             title = 'End of year position for simulated scenario', 
                             subtitle = sfinalposition,
                             caption = paste0('Go to court in year ', s$year_court, ' no exploration'), 
                             ci = .5
  )
  plot_file_name2 <- paste0('s1_eoy_position_line_court_',s$year_court,'_no_exploration.png')
  png(paste0('./img/',plot_file_name2))
  print(p2)
  
  p3 <- plot_yearly_boxplots(s$pnl, 
                             title = 'Yearly P&L position for simulated scenario', 
                             subtitle = sfinalposition,
                             caption = paste0('Go to court in year ', s$year_court, ' no exploration')
  )
  plot_file_name3 <- paste0('s1_pnl_box_court_',s$year_court,'_no_exploration.png')
  png(paste0('./img/',plot_file_name3))
  print(p3)
  dev.off()
  
  dev.off()
  
}



#### Scenario 2: in house exploration ####
scenarios2 <- list() 
for (year_court in 1:5){ 
  
  exploration_plan = data.frame(year = c(1,2,3,4,5),
                                teams = c(1,1,1,1,1))
  
  s <- whatif(year_court, exploration_plan)
  s[['exploration_plan']] <- exploration_plan
  s[['year_court']] <- year_court
  
  scenarios2[[paste0('year_court_', year_court)]] <-s
}


for (s in scenarios2){
  print(s$year_court)
  sfinalposition <- plot_label_quantiles(s$final_position)
  p1 <- plot_yearly_boxplots(s$eoy_position, 
                             title = 'End of year position for simulated scenario', 
                             subtitle = sfinalposition,
                             caption = paste0('Go to court in year ', s$year_court, ' inhouse exploration')
  )
  plot_file_name1 <- paste0('s2_eoy_position_box_court_',s$year_court,'_inhouse_exploration.png')
  png(paste0('./img/',plot_file_name1))
  print(p1)
  dev.off()
  p2 <- plot_yearly_lines(s$eoy_position, 
                          title = 'End of year position for simulated scenario', 
                          subtitle = sfinalposition,
                          caption = paste0('Go to court in year ', s$year_court, ' inhouse exploration'), 
                          ci = .5
  )
  plot_file_name2 <- paste0('s2_eoy_position_line_court_',s$year_court,'_inhouse_exploration.png')
  png(paste0('./img/',plot_file_name2))
  print(p2)
  
  
  p3 <- plot_yearly_boxplots(s$pnl, 
                             title = 'Yearly P&L position for simulated scenario', 
                             subtitle = sfinalposition,
                             caption = paste0('Go to court in year ', s$year_court, ' inhouse exploration')
  )
  plot_file_name3 <- paste0('s2_pnl_box_court_',s$year_court,'_inhouse_exploration.png')
  png(paste0('./img/',plot_file_name3))
  print(p3)
  dev.off()
  
  dev.off()
  
}





#### Scenario 3: external exploration ####
scenarios3 <- list() 
for (year_court in 1:5){ 
  
  exploration_plan = data.frame(year = c(1,2,3,4,5),
                                teams = c(3,2,2,2,1))
  
  s <- whatif(year_court, exploration_plan)
  s[['exploration_plan']] <- exploration_plan
  s[['year_court']] <- year_court
  
  scenarios3[[paste0('year_court_', year_court)]] <-s
}


for (s in scenarios3){
  print(s$year_court)
  sfinalposition <- plot_label_quantiles(s$final_position)
  p1 <- plot_yearly_boxplots(s$eoy_position, 
                             title = 'End of year position for simulated scenario', 
                             subtitle = sfinalposition,
                             caption = paste0('Go to court in year ', s$year_court, ' external exploration')
  )
  plot_file_name1 <- paste0('s3_eoy_position_box_court_',s$year_court,'_external_exploration.png')
  png(paste0('./img/',plot_file_name1))
  print(p1)
  dev.off()
  p2 <- plot_yearly_lines(s$eoy_position, 
                          title = 'End of year position for simulated scenario', 
                          subtitle = sfinalposition,
                          caption = paste0('Go to court in year ', s$year_court, ' external exploration'), 
                          ci = .5
  )
  plot_file_name2 <- paste0('s3_eoy_position_line_court_',s$year_court,'_external_exploration.png')
  png(paste0('./img/',plot_file_name2))
  print(p2)
  
  
  p3 <- plot_yearly_boxplots(s$pnl, 
                             title = 'Yearly P&L position for simulated scenario', 
                             subtitle = sfinalposition,
                             caption = paste0('Go to court in year ', s$year_court, ' external exploration')
  )
  plot_file_name3 <- paste0('s3_pnl_box_court_',s$year_court,'_external_exploration.png')
  png(paste0('./img/',plot_file_name3))
  print(p3)
  dev.off()
  
  dev.off()
  
}

sfinalposition <- plot_label_quantiles(scenarios3$year_court_5$final_position, ci = .95)

p3_year5 <- plot_yearly_boxplots(scenarios3$year_court_5$pnl, 
                           title = 'Yearly P&L position for simulated scenario', 
                           subtitle = sfinalposition,
                           caption = '')

p3_year5_filename <- paste0('s3_pnl_box_court_',s$year_court,'_external_exploration_report.png')
png(paste0('./img/',p3_year5_filename))
print(p3_year5)
dev.off()
