
source('at1b_13184383_final.R')


year_court = 1
exploration_plan = data.frame(year = c(1,2,3,4,5),
                              teams = c(3,1,1,1,1))

sims1 <- whatif(year_court, exploration_plan)

plot_yearly_lines(sims1$eoy_position)
plot_yearly_lines(sims1$eoy_position, ci=.95)
plot_yearly_lines(sims1$eoy_position, ci=.8)

plot_yearly_lines(sims1$eoy_position)

plot_yearly_boxplots(sims1$claim_proceeds, title = 'Claim proceeds', caption = 'including legal cost')
# plot_yearly_boxplots(sims1$potential_proceeds, title = 'Potential proceeds', caption = 'before legal cost')
plot_yearly_boxplots(sims1$exploration$claim_pnl, title = 'Future claims P&L', caption = '3 exploration teams in year 1')
plot_yearly_boxplots(sims1$pnl, title='Yearly Profit and Loss', caption = print_quantiles('year1 P&L', sims1$pnl$year1))
plot_yearly_boxplots(sims1$eoy_position, caption = print_quantiles('year1 position', sims1$eoy_position$year1))
print_quantiles('year1 position', sims1$eoy_position$year1)
print_quantiles('year5 position', sims1$eoy_position$year5)
print_quantiles('final position', sims1$final_position)
  # rownames_to_column(as.character(c(.025, .05,.1,.5,.9,.95,.975)))



# scenario 2 -- 2 teams at the begining 