# AT1B: Digging for Gold 
# Mutaz Abu Ghazaleh 
# 13184383

library(tidyverse)

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
  
  # samples_summary = round(summary(samples),2)
  # samples_summary['sd'] = round(sd(samples),2)
  # samples_summary['var'] = round(var(samples), 2)
  # samples_density = density(samples)
  # 
  # hist(samples,breaks = hist_breaks, probability = TRUE,
  #      main = paste0("Density of value in trials - ", invFn),
  #      xlab="Simulated value")
  # lines(x = samples_density, col='black', lwd=2)
  # abline(v=samples_summary["Median"], lty = 2, lwd=2, col = "grey80")
  # abline(v=samples_summary["Mean"], lty = 2, lwd=2, col = "blue")
  # abline(v = samples_summary["Mean"]+samples_summary['sd'], col='blue3', lwd=1, lty=2)
  # abline(v = samples_summary["Mean"]-samples_summary['sd'], col='blue3', lwd=1, lty=2)
  # abline(v = samples_summary["Mean"]-2*samples_summary['sd'], col='skyblue', lwd=1, lty=2)
  # abline(v = samples_summary["Mean"]+2*samples_summary['sd'], col='skyblue', lwd=1, lty=2)
  # text(
  #   x = samples_density$x[which.max(samples_density$y)] + samples_summary['sd']/2, 
  #   y = max(samples_density$y),
  #   labels = paste0('vmax: ', round(samples_density$x[which.max(samples_density$y)])))
  # points(
  #   x = samples_density$x[which.max(samples_density$y)], 
  #   y = max(samples_density$y), 
  #   lwd = 2 )
  # title(sub = paste("Mean (in blue) ", samples_summary["Mean"]))
  # print(samples_summary)
  # print(paste0('simulated vmax: ', round(samples_density$x[which.max(samples_density$y)])))
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

i = 2
yearn_label = paste0( 'year', i)
yearn_prev = paste0( 'year', i-1)
yearn <- v %>% filter(year == year_label)

price_simulation[year_label] <- price_simulation[yearn_prev] + inv_triangle_cdf(
  P = runif(n_trials), 
  vmin = yearn$vmin, 
  vml = yearn$vml,
  vmax = yearn$vmax) %>% round(digits=2)

price_simulation %>% glimpse()
