##### ####################################################
#####                                               ######
#####           Stan model prep (approval)
#####                                               ######
##### ####################################################

# init ------------------------------------------------------------

rm(list=ls())

# Load libraries

library(rstan) # CRAN v2.19.2

## Source stan models

source("stancode/1_stan_simple_approval.R")
source("stancode/2_stan_alpha_approval.R")  
source("stancode/3_stan_delta_approval.R")
source("stancode/4_stan_full_approval.R")

# Compile the models

simple_stan_model_approval <- stan_model(model_code = stancode_simple) 
alpha_stan_model_approval <- stan_model(model_code = stancode_alpha)
delta_stan_model_approval <- stan_model(model_code = stancode_delta)
full_stan_model_approval <- stan_model(model_code = stancode_full)

save(simple_stan_model_approval, 
     alpha_stan_model_approval, 
     delta_stan_model_approval, 
     full_stan_model_approval,
     file = "working/saved_stan_models_approval.Rdata")

