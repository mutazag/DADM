#### Scanrios and Exploration ####
# AT2: Business Case Development
# AI-Assisted Accelerated MRI 

# Mutaz Abu Ghazaleh: 13184383 
# Jay Radhakrishnan: 13037686
# Anand KC: 13386331
# Andrew Barbour: 12822589
# Chris Garces: 12473310


source(file = 'at2_ai_assisted_mri_wip.R')


##### Interal Development Scenario #####

scenario1 <- whatif()
# plot_label_quantiles(scenario1$position$final, ci=.9)
plot_periods_lines(scenario1$position$yearly, ci=.9, 
                   title = 'Financial Outlook',
                   subtitle = 'all internal development',
                   caption = plot_label_quantiles(scenario1$position$final, ci=.9))
plot_periods_lines(scenario1$position$qtrly, ci=.5,
                   title = 'Quarterly for simulated scenario', 
                   subtitle = 'all internal development',
                   caption = plot_label_quantiles(scenario1$position$final, ci=.9))
plot_periods_boxplots(scenario1$pnl$qtrly,
                      title = 'P&Ls for Quarterly periods', 
                      subtitle = 'all internal development',
                      caption = 'initial investment is not reflected')



##### external Development Scenario #####

scenario2 <- whatif(rnd_external = TRUE)
# plot_label_quantiles(scenario2$position$final, ci=.9)
plot_periods_lines(scenario2$position$yearly, ci=.8, 
                   title = 'Financial Outlook',
                   subtitle = 'with external development',
                   caption = paste('by year 3:', plot_label_quantiles(scenario2$position$yearly$year03, ci=.5)))
plot_label_quantiles(scenario2$position$final, ci=.5)

plot_periods_lines(scenario2$position$qtrly, ci=.9,
                   title = 'Quarterly for simulated scenario', 
                   subtitle = 'with external development',
                   caption = plot_label_quantiles(scenario2$position$final, ci=.9))
plot_periods_boxplots(scenario2$pnl$qtrly,
                      title = 'P&Ls for Quarterly periods', 
                      subtitle = 'with external development',
                      caption = 'initial investment is not reflected')



plot_periods_boxplots(scenario2$cost_rnd, title = 'Cost of R&D')
plot_periods_boxplots(scenario2$cost_trials, title = 'Cost of Trials')
plot_periods_boxplots(scenario2$sales$revenue, title = 'Revenue')
plot_periods_boxplots(scenario2$sales$cost_of_sales, title = 'Cost of Sales')
plot_periods_boxplots(scenario2$rnd_completes, 
                      title = 'R&D duration', 
                      subtitle = 'R&D is expected to complete in 1 to 1.5 years') + 
  coord_flip() + 
  scale_y_continuous(labels = c(0,.5,1,1.5,2)) + 
  scale_x_discrete(labels = c())

plot_periods_boxplots(scenario2$trials_complete, 
                      title = 'Trials and FDA Approvals', 
                      subtitle = 'Approvals will be secured by the end of second year') + 
  coord_flip() + 
  scale_y_continuous() + 
  scale_x_discrete(labels = c())
