# AT1B: Digging for Gold 
# Mutaz Abu Ghazaleh 
# 13184383

library(tidyverse)
library(gridExtra)

#### Inverse Sampling Transform ####

# Sampling Inverse transformation sampling takes
# uniform samples of a number r between 0 and 1, interpreted as
# a probability, and then returns the largest number x from the
# domain of the distribution

# here we will code inverse sampling functions for triangular and discrete
# distribution


inv_triangle_cdf <- function (P, vmin, vml, vmax){
  # inverse triangle cdf is used to translate probabilities based on triangular
  # PDF, stuiable for 3-point estimate simulation 
  Pvml <- (vml-vmin)/(vmax-vmin)
  
  return(ifelse(P < Pvml,
                vmin + sqrt(P*(vml-vmin)*(vmax-vmin)),
                vmax - sqrt((1-P)*(vmax-vml)*(vmax-vmin))))
}

plot_distribution <- function(samples=rnorm(1000), label='normal distribution', hist_breaks = 20){
  samples_summary = round(summary(samples),2)
  samples_summary['sd'] = round(sd(samples),2)
  samples_summary['var'] = round(var(samples), 2)
  samples_density = density(samples)
  
  
  as_data_frame(samples) %>% ggplot() + 
    geom_histogram(aes(value,after_stat(density)), fill='grey90',color='grey85', bins=50 ) +
    geom_density(aes(value), col='black', lwd=1) + 
    geom_vline(aes(xintercept=samples_summary["Median"]),lty = 2, lwd=1, col = "grey80")  + 
    geom_vline(aes(xintercept=samples_summary["Mean"]), lty = 2, lwd=1, col = "blue") +
    geom_vline(aes(xintercept=samples_summary["Mean"]+samples_summary['sd']), col='blue3', lwd=1, lty=2) +
    geom_vline(aes(xintercept=samples_summary["Mean"]-samples_summary['sd']), col='blue3', lwd=1, lty=2) +
    geom_vline(aes(xintercept=samples_summary["Mean"]-2*samples_summary['sd']), col='skyblue', lwd=1, lty=2) +
    geom_vline(aes(xintercept=samples_summary["Mean"]+2*samples_summary['sd']), col='skyblue', lwd=1, lty=2) +
    annotate(
      geom = 'text', 
      x = samples_density$x[which.max(samples_density$y)] + samples_summary['sd']/2, 
      y = max(samples_density$y),
      label = paste0('vmax: ', round(samples_density$x[which.max(samples_density$y)]))) +
    annotate(
      geom = 'point', 
      x = samples_density$x[which.max(samples_density$y)], 
      y = max(samples_density$y), 
      lwd = 2 ) +
    labs( title = paste0("Density of value in trials - ", label), 
          subtitle = "Simulated value", 
          caption = paste("Mean (in blue) ", samples_summary["Mean"])) +
    theme_bw() -> p

  print(samples_summary)
  print(paste0('simulated vmax: ', round(samples_density$x[which.max(samples_density$y)])))
  
  return(p)
  
}



# test inv_triangle function
test_inv_triangle_cdf <- function(
  trials=10000, 
  invFn = "inv_triangle_cdf", 
  vmin = -100, vml = 50, vmax = 350,
  hist_breaks = 20){
  # testing inv_triangle_cdf functions to compare the two methods
  P = runif(trials)
  samples = get(invFn)(P= P, vmin = vmin, vml = vml, vmax = vmax)
  distro_plot <- plot_distribution(samples = samples, label = invFn, hist_breaks = 50)
  distro_plot()
}


#### load simulation paramaters #### 

sim_params <- read.csv('./parameters.csv')
sim_params %>% group_by(cat) %>% summarise(count = n())
n_trials = sim_params %>% filter(cat == 'simulation', param=='n_trials') %>% select(vmin) %>% first()
print(n_trials)

#### simulate price change #### 

v <- sim_params %>% filter(cat == 'price_change') %>%  select(year=param, vmin, vml, vmax)
print(v)

price_simulation <- data.frame(year1= rep((v %>% filter(year == 'year1'))$vmin, n_trials))

price_simulation %>% head()


for ( i in 2:5){
  yearn_label = paste0( 'year', i)
  yearn_prev = paste0( 'year', i-1)
  yearn <- v %>% filter(year == yearn_label)
  
  price_simulation[yearn_label] <- price_simulation[yearn_prev] + inv_triangle_cdf(
    P = runif(n_trials), 
    vmin = yearn$vmin, 
    vml = yearn$vml,
    vmax = yearn$vmax) %>% round(digits=2)
  # 
  # price_simulation %>% glimpse()

}


plots <- list()
xlim <- c(min(price_simulation), max(price_simulation))
for (i in 2:5){
  yearn_label = paste0( 'year', i)
  plots[[yearn_label]] <- plot_distribution(price_simulation[[yearn_label]], label = yearn_label) + lims(x=xlim)
  }
grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow=4)


price_simulation %>% pivot_longer(
  cols=c(year1, year2, year3, year4, year5), 
  names_to='year', 
  values_to='price') %>% ggplot() + 
  geom_boxplot(aes(y=price, x=year))


#### simulate new claim output year over year ####
v_claim <- sim_params %>% filter(cat == 'new_claim')

claim_year1_output_estimate <- v_claim %>% filter(param == 'predicted') %>% select(vmin, vml, vmax)
claim_year_on_year_change <- v_claim %>% filter(param == 'change_yoy') %>% select(vmin, vml, vmax)
claim_cost_ops <- v_claim %>% filter(param == 'cost_ops') %>% select(vmin, vml, vmax)
claim_cost_sales <- v_claim %>% filter(param == 'cost_sales') %>% select(vmin, vml, vmax) %>% first()


output_simulation <- data.frame(
  year1= inv_triangle_cdf(
    P=runif(n_trials), 
    vmin=claim_year1_output_estimate$vmin,
    vml=claim_year1_output_estimate$vml,
    vmax=claim_year1_output_estimate$vmax))


output_simulation %>% head()


for ( i in 2:5){
  yearn_label = paste0( 'year', i)
  yearn_prev = paste0( 'year', i-1)
  yearn <- claim_year_on_year_change
  
  output_change <-  inv_triangle_cdf(
    P = runif(n_trials), 
    vmin = yearn$vmin, 
    vml = yearn$vml,
    vmax = yearn$vmax)
  
  output_simulation[yearn_label] <- output_simulation[yearn_prev] * (1+output_change) %>% round(digits=2)
  # 
  # price_simulation %>% glimpse()
  
}


plots <- list()
xlim <- c(min(output_simulation), max(output_simulation))
for (i in 1:5){
  yearn_label = paste0( 'year', i)
  plots[[yearn_label]] <- plot_distribution(output_simulation[[yearn_label]], label = yearn_label) + lims(x=xlim)
}

grid.arrange(plots[['year1']], plots[['year2']], plots[['year3']], plots[['year4']], plots[['year5']], nrow=5)


output_simulation %>% pivot_longer(
  cols=c(year1, year2, year3, year4, year5), 
  names_to='year', 
  values_to='output') %>% ggplot() + 
  geom_boxplot(aes(y=output, x=year)) + 
  labs(title = 'output simulation')
  

#### simulate revenue for 5 years #### 

price_simulation %>% head()
output_simulation %>% head()



v_claim <- sim_params %>% filter(cat == 'new_claim')
claim_cost_ops <- v_claim %>% filter(param == 'cost_ops') %>% select(vmin, vml, vmax)
claim_cost_sales <- v_claim %>% filter(param == 'cost_sales') %>% select(vmin) %>% first()

revenue1_simulation <- data.frame(year1=rep(0,n_trials))
fixedcost_simulation <- data.frame(year1=rep(0,n_trials))

revenue1_simulation %>% head()
fixedcost_simulation %>% head()

for (i in 1:5){
  yearn_label = paste0( 'year', i)
  
  fixedcost_simulation[yearn_label] <- inv_triangle_cdf(
      P=runif(n_trials), 
      vmin=claim_cost_ops$vmin,
      vml=claim_cost_ops$vml,
      vmax=claim_cost_ops$vmax)
  
  revenue1_simulation[yearn_label] <- 
    ((1-claim_cost_sales) * output_simulation[yearn_label] * price_simulation[yearn_label]) - fixedcost_simulation[yearn_label]
}


revenue1_simulation %>% pivot_longer(
  cols=c(year1, year2, year3, year4, year5), 
  names_to='year', 
  values_to='revenue1') %>% ggplot() + 
  geom_boxplot(aes(y=revenue1, x=year)) + 
  labs(title = 'revenue1 simulation') + 
  scale_y_continuous(labels = scales::comma, limits= c(-10000,4000000)) 


min(revenue1_simulation)


price_simulation %>% head()
output_simulation %>% head()
revenue1_simulation %>% head()

## inverse cdf to answer questions like:  what is the likelihood that my revenue
## by end of 5 years is 16Mil


#### legal battle ####

#simulate winning or losing for each trial with  .4 prob winning

sum(sample(x=c(T,F), size=10000, replace=TRUE, prob=c(.4,.6)))
sum(rbinom(n=10000,size=1,prob=.4))


# simulate probaility to win (T/F) 

# simulate legal cost 

# subtract legal cost from gross for year if not yet tried
# zero out yearly revenue after year of trying if win probability is false

#legal simulation parametrs
v_legal <- sim_params %>% filter(cat == 'legal')
legal_cost_annual <- v_legal %>% filter(param=='cost_annual') %>% select(vmin, vml, vmax)
legal_resolve_in_favor <- v_legal %>% filter(param=='resolve_in_favor') %>% select(vmin) %>% first()

# initialise legal simulation results 
legal_cost_simulation <- data.frame(year1=rep(0,n_trials))
legal_resolve_in_favor_simulation <- data.frame(year1=rep(0,n_trials))
# legal resolve is simulated for every year independently but only used once
# based on strategy to resolve or not for a given year

for (i in 1:5){
  yearn_label = paste0( 'year', i)
  
  legal_cost_simulation[yearn_label] <- inv_triangle_cdf(
    P=runif(n_trials), 
    vmin=legal_cost_annual$vmin,
    vml=legal_cost_annual$vml,
    vmax=legal_cost_annual$vmax)
  
  legal_resolve_in_favor_simulation[yearn_label] <- rbinom(n=n_trials, size=1, prob=legal_resolve_in_favor)
 
}

# plot legal cost simuation results
legal_cost_simulation %>% pivot_longer(
  cols=c(year1, year2, year3, year4, year5), 
  names_to='year', 
  values_to='legal_cost') %>% ggplot() + 
  geom_boxplot(aes(y=legal_cost, x=year)) + 
  labs(title = 'legal_cost simulation') + 
  scale_y_continuous(labels = scales::comma, limits= c(500000,1000000)) 



legal_cost_simulation[1:100,] %>% mutate(i=row_number()) %>% pivot_longer(
    cols=c(year1, year2, year3, year4, year5), 
    names_to='year', 
    values_to='legal_cost') %>%
  ggplot(aes(x=i)) + geom_line(aes(y=legal_cost, col=year), lwd=1) + 
  scale_y_continuous(labels = scales::comma, limits= c(500000,1000000)) 


xlim <- c(min(legal_cost_simulation), max(legal_cost_simulation))
for (i in 1:5){
  yearn_label = paste0( 'year', i)
  plots[[yearn_label]] <- plot_distribution(legal_cost_simulation[[yearn_label]], label = yearn_label) + lims(x=xlim)
}
grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]],plots[[5]], nrow=5)

legal_resolve_in_favor_simulation %>% summarise_all(sum)

