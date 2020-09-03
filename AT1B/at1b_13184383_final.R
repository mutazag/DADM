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
  
  print(p)
  switch (p$type,
    '1-point' = p <- p %>% select(vmin) %>% first(), 
    '3-point' = p <- p %>% select(vmin, vml, vmax)
  )
  # select(vmin) %>% first()
  # %>% select(vmin, vml, vmax)
  
  print(p)
}


