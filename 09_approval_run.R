##### ####################################################
#####                                               ######
#####             Stan model run (approval)
#####                                               ######
##### ####################################################

# init ------------------------------------------------------------

rm(list=ls())

# Load libraries

library(data.table) # CRAN v1.12.2
library(rstan) # CRAN v2.19.2

adapt_engaged <- F
runSimple <- T
runAlpha <- T
runDelta <- T
runFull <- T

## Load data

load("working/saved_stan_models_approval.Rdata")
load("working/forStan_approval.Rdata")

## Specify models

full_variables <- c("alpha", "delta", "gamma", "alpha_0",
                    "mu_alpha","mu_alpha_new_mp", "mu_alpha_cabinet","mu_alpha_margin","mu_alpha_party",
                    "mu_gamma", "mu_gamma_new_mp", "mu_gamma_cabinet","mu_gamma_margin","mu_gamma_party",
                    "mu_delta",
                    "sigma_alpha", "sigma_delta", "sigma_gamma", "deviance", "log_likelihood")

simple_variables <- full_variables[grepl("log_likelihood|deviance|alpha_0|gamma",full_variables)]

alpha_variables <- full_variables[grepl("log_likelihood|deviance|alpha_0|gamma|alpha",full_variables)]

delta_variables <- full_variables[grepl("log_likelihood|deviance|alpha_0|gamma|delta",full_variables)]

## Estimate models and calculate DIC

if(runSimple){
  
  variational_posterior_simple <- vb(
    object = simple_stan_model_approval, 
    data = forStan,
    pars = simple_variables,
    iter = 10000, seed = 2118, adapt_engaged = adapt_engaged, eta  = 1)
  
  deviance_chain <- extract(variational_posterior_simple, "deviance")[[1]]
  
  dic_simple <- .5*var(deviance_chain) + mean(deviance_chain)
  
  save(variational_posterior_simple, file= "working/stanOut/approval/stanOut_simple.Rdata")
  save(dic_simple, file= "working/stanOut/approval/dic_simple.Rdata")
  
}

if(runAlpha){
  
  variational_posterior_alpha <- vb(
    object = alpha_stan_model_approval, 
    data = forStan,
    pars = alpha_variables,
    iter = 10000, seed = 2118, adapt_engaged = adapt_engaged, eta  = 1)
  
  deviance_chain <- extract(variational_posterior_alpha, "deviance")[[1]]
  
  dic_alpha <- .5*var(deviance_chain) + mean(deviance_chain)
  
  save(variational_posterior_alpha, file= "working/stanOut/approval/stanOut_alpha.Rdata")
  save(dic_alpha, file= "working/stanOut/approval/dic_alpha.Rdata")
  
}

if(runDelta){
  
  variational_posterior_delta <- vb(
    object = delta_stan_model_approval, 
    data = forStan,
    pars = delta_variables,
    iter = 10000, seed = 2118, adapt_engaged = adapt_engaged, eta  = 1)
  
  deviance_chain <- extract(variational_posterior_delta, "deviance")[[1]]
  
  dic_delta <- .5*var(deviance_chain) + mean(deviance_chain)
  
  save(variational_posterior_delta, file= "working/stanOut/approval/stanOut_delta.Rdata")
  save(dic_delta, file= "working/stanOut/approval/dic_delta.Rdata")
  
}

if(runFull){
  
  variational_posterior_full <- vb(
    object = full_stan_model_approval, 
    data = forStan,
    pars = full_variables,
    iter = 10000, seed = 2118, adapt_engaged = adapt_engaged, eta  = 1)
  
  deviance_chain <- extract(variational_posterior_full, "deviance")[[1]]
  
  dic_full <- .5*var(deviance_chain) + mean(deviance_chain)
  
  save(variational_posterior_full, file= "working/stanOut/approval/stanOut_full.Rdata")
  save(dic_full, file= "working/stanOut/approval/dic_full.Rdata")

  
}

