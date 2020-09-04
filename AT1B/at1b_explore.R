
source('at1b_13184383_final.R')


year_court = 2
exploration_plan = data.frame(year = c(1,2,3,4,5),
                              teams = c(3,2,2,2,1))

sims1 <- whatif(year_court, exploration_plan)



as.data.frame(sims1$pnl) %>% head() %>% summarise_all(list(m1 = min, m2=max))


as.data.frame(sims1$pnl) %>% head() %>% pivot_longer(cols = year1:year5) %>% 
  group_by(name) %>% 
  summarise(.funs=quantile, c(.1,.5,.9))


as.data.frame(sims1$pnl) %>% 
  summarise(across(year1:year5, quantile, c(.025, .05,.1,.5,.9,.95,.975))) %>% 
  'rownames<-' (c("0:2.5%", "1:5%","2:10%","3:50%","4:90%", "5:95%","6:97.5%")) %>% 
  rownames_to_column('quantiles') %>% 
  pivot_longer(cols=year1:year5) %>% 
  ggplot(aes(x=name, y=value, group=quantiles, colour=quantiles)) +
  geom_line() + 
  geom_point() + 
  scale_y_continuous(labels = scales::comma)



as.data.frame(sims1$pnl) %>% 
  summarise(across(year1:year5, quantile, 1-c(.025, .05,.1,.5,.9,.95,.975))) %>% 
  'rownames<-' (c("0:2.5%", "1:5%","2:10%","3:50%","4:90%", "5:95%","6:97.5%")) %>% 
  rownames_to_column('quantiles') %>% 
  pivot_longer(cols=year1:year5) %>% 
  ggplot(aes(x=name, y=value, group=quantiles, colour=quantiles)) +
  geom_line() + 
  geom_point() + 
  scale_y_continuous(labels = scales::comma)

  print_quantiles('final position', sims1$final_position)
  
  # rownames_to_column(as.character(c(.025, .05,.1,.5,.9,.95,.975)))


as.data.frame(sims1$pnl) %>% head() %>% 
  summarise(across(year1:year5,ecdf))


quantile((as.data.frame(sims1$pnl) %>% head())$year1)

y1 <- (as.data.frame(sims1$pnl) %>% head())$year1
y1
median(y1)

quantile(y1, c(.5))


claim_proceeds_budget_fx <- ecdf(sims$claim_proceeds$year1 + sims$budget)
plot(claim_proceeds_budget_fx)

# proceeds in year 5 of exploration are almost 70% likely not to generate anything by year 5 -- not clear tos one 
plot(ecdf(s$exploration$claim_proceeds$year5))
plot(ecdf(s$exploration$potential_proceeds$year5))

ecdf(s$exploration$potential_proceeds$year5)(2e+06) # 15 percent to generate up to 2 mil potentially 
ecdf(s$exploration$claim_proceeds$year5)(2e+06) # 41% will be under 2mill
ecdf(s$exploration$claim_proceeds$year5)(1e+06) # = 30% we need to be looking at 1- prob, this means that there is 70% that we ill conly produce up to 1 mil
ecdf(s$exploration$claim_proceeds$year5)(3e+06) # only 45% chance that will produce up to 3mil



quantile(ecdf(s$exploration$claim_proceeds$year5), c(0.05, .95), type=7)
# 90% confidence that we will generate between 0 and 6 mill 

ecdf(s$exploration$claim_pnl$year2)(1e06)
#[1] 0.8731 -> only 13% probability to be in the profit by 1mil by year 2

ecdf(s$exploration$claim_pnl$year1)(1e06)
#[1] 1 -> zero likelyhood to turn any profits in year 1

ecdf(s$exploration$claim_pnl$year5)(1e06)
#[1] 0.306 -> 70% to make a profit of atleast 1mil


1 - ecdf(sims$final_position)(5e+6)
# [1] 0.9997 -- 99% probabiolity we will make more than 5mill
1 - ecdf(sims$final_position)(17e+6)
# [1] 0.465 to make more than 17 mill
1 - ecdf(sims$final_position)(10e+6)
# [1] 0.8304 and 83 to make more than 10mil

plot(ecdf(sims$final_position))
