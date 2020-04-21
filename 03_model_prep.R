##### ####################################################
#####                                               ######
#####                   Stan models
#####                                               ######
##### ####################################################

# init ------------------------------------------------------------

rm(list=ls())

# Load libraries

library(rstan) # CRAN v2.19.2

######
### Participation models

# Source stan models

source("stancode/1_stan_simple.R")  
source("stancode/2_stan_alpha.R")  
source("stancode/3_stan_delta.R")
source("stancode/4_stan_alpha_delta.R")
source("stancode/5_stan_full.R")
source("stancode/6_stan_full_no_party.R")
source("stancode/7_stan_full_double_gamma_with_party.R")

# Compile the models

simple_stan_model <- stan_model(model_code = stancode_simple)
alpha_stan_model <- stan_model(model_code = stancode_alpha)
delta_stan_model <- stan_model(model_code = stancode_delta)
alpha_delta_stan_model <- stan_model(model_code = stancode_alpha_delta)
full_stan_model <- stan_model(model_code = stancode_full)
full_stan_model_no_party <- stan_model(model_code = stancode_full_no_party)
full_stan_model_double_gamma <- stan_model(model_code = stancode_double_gamma_with_party)

save(simple_stan_model, 
     alpha_stan_model, 
     delta_stan_model, 
     alpha_delta_stan_model, 
     full_stan_model,
     full_stan_model_no_party,
     full_stan_model_double_gamma,
     file = "working/saved_stan_models.Rdata")

######
### Approval models

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

