
source('at1b_13184383_final.R')


year_court = 1
exploration_plan = data.frame(year = c(1,2,3,4,5),
                              teams = c(3,1,1,1,1))

sims1 <- whatif(year_court, exploration_plan)

plot_yearly_lines(sims1$eoy_position)
plot_yearly_lines(sims1$eoy_position, ci=.95)
plot_yearly_lines(sims1$eoy_position, ci=.8)

plot_yearly_lines(sims1$eoy_position)

plot_yearly_boxplots(sims1$claim_proceeds, title = 'Claim proceeds', subtitle='court year 1',caption = 'including legal cost')
plot_yearly_boxplots(sims1$pnl, title='Yearly Profit and Loss', caption = print_quantiles('year1 P&L', sims1$pnl$year1))
plot_yearly_boxplots(sims1$eoy_position, subtitle='court year 1', caption = print_quantiles('year1 position', sims1$eoy_position$year1))

plot_yearly_boxplots(sims1$exploration$claim_pnl, title = 'Future claims P&L', caption = '3 exploration teams in year 1')
print_quantiles('year1 position', sims1$eoy_position$year1)
print_quantiles('year5 position', sims1$eoy_position$year5)
print_quantiles('final position', sims1$final_position)




# scenario 2 -- court year 2
year_court = 2
exploration_plan = data.frame(year = c(1,2,3,4,5),
                              teams = c(3,1,1,1,1))
sims2 <- whatif(year_court, exploration_plan)
plot_yearly_boxplots(sims2$claim_proceeds, title = 'Claim proceeds', subtitle='court year 2', caption = 'including legal cost')
plot_yearly_boxplots(sims2$pnl, title='Yearly Profit and Loss', caption = print_quantiles('year1 P&L', sims1$pnl$year1))
plot_yearly_boxplots(sims2$eoy_position, subtitle='court year 2', caption = print_quantiles('year1 position', sims1$eoy_position$year1))
print_quantiles('final position', sims2$final_position)



# scenario 3 -- court year 1
year_court = 1
exploration_plan = data.frame(year = c(1,2,3,4,5),
                              teams = c(1,1,1,1,1))
sims3 <- whatif(year_court, exploration_plan)
plot_yearly_boxplots(sims3$claim_proceeds, title = 'Claim proceeds', subtitle='court year 1 - not enough exploration', caption = 'including legal cost')
plot_yearly_boxplots(sims3$pnl, title='Yearly Profit and Loss', caption = print_quantiles('year1 P&L', sims1$pnl$year1))
plot_yearly_boxplots(sims3$eoy_position, subtitle='court year 1 - not enough exploration', caption = print_quantiles('year1 position', sims1$eoy_position$year1))
print_quantiles('final position', sims3$final_position)




# scenario 4-- court year 5
year_court = 5
exploration_plan = data.frame(year = c(1,2,3,4,5),
                              teams = c(1,1,1,1,1))
sims4 <- whatif(year_court, exploration_plan)
plot_yearly_boxplots(sims4$claim_proceeds, title = 'Claim proceeds', subtitle='court year 5 - not enough exploration', caption = 'including legal cost')
plot_yearly_boxplots(sims4$pnl, title='Yearly Profit and Loss', caption = print_quantiles('year1 P&L', sims1$pnl$year1))
plot_yearly_boxplots(sims4$eoy_position, subtitle='court year 5 - not enough exploration', caption = print_quantiles('year1 position', sims1$eoy_position$year1))
print_quantiles('final position', sims4$final_position)

# chance to break even at end of year 5
ecdf(sims1$final_position)(0)
ecdf(sims2$final_position)(0)
ecdf(sims3$final_position)(0)
ecdf(sims4$final_position)(0)


# finishing with 10mill at end of year 5
ecdf(sims1$final_position)(10e6)
ecdf(sims2$final_position)(10e6)
ecdf(sims3$final_position)(10e6)
ecdf(sims4$final_position)(10e6)


# scenario 5-- tank
year_court = 1
exploration_plan = data.frame(year = c(1,2,3,4,5),
                              teams = c(0,0,0,0,0))
sims5 <- whatif(year_court, exploration_plan)
plot_yearly_boxplots(sims5$potential_proceeds, title = 'Potential proceeds', subtitle='tank', caption = 'NOT including legal cost')
plot_yearly_boxplots(sims5$claim_proceeds, title = 'Claim proceeds', subtitle='tank', caption = 'including legal cost')
plot_yearly_boxplots(sims5$pnl, title='Yearly Profit and Loss', caption = print_quantiles('year1 P&L', sims1$pnl$year1))
plot_yearly_boxplots(sims5$eoy_position, subtitle='tank', caption = print_quantiles('year1 position', sims1$eoy_position$year1))
print_quantiles('final position', sims5$final_position)

plot_yearly_lines(sims5$eoy_position, ci = .5)
