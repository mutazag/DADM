
source('at1b_13184383_final.R')


year_court = 2
exploration_plan = data.frame(year = c(1,2,3,4,5),
                              teams = c(1,2,2,2,2))

sims1 <- whatif(year_court, exploration_plan)
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
