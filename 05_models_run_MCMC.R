##### ####################################################
#####                                               ######
#####                   Stan model
#####                                               ######
##### ####################################################

# init ------------------------------------------------------------

rm(list=ls())

# Load libraries

library(data.table) # CRAN v1.12.2 
library(rstan) # CRAN v2.19.2

HMCSample <- 400
HMCBurn <- 100
HMCChains <- 3

options(mc.cores = 3)

## Load data

load("working/mp_by_debate.Rdata")
load("working/topicmodel/ldaOut_10.Rdata")
load("working/saved_stan_models.Rdata")
load("working/text_objects.Rdata")

## Specify models

full_variables <- c("alpha", "delta", "gamma", "alpha_0", "Beta",
                    "mu_alpha","mu_alpha_new_mp", "mu_alpha_cabinet","mu_alpha_margin","mu_alpha_party",
                    "mu_gamma", "mu_gamma_new_mp", "mu_gamma_cabinet","mu_gamma_margin","mu_gamma_party",
                    "mu_delta", "mu_delta_petition",
                    "sigma_alpha", "sigma_delta", "sigma_gamma", "sigma_beta")

simple_variables <- full_variables[grepl("log_likelihood|deviance|alpha_0|gamma",full_variables)]

alpha_delta_variables <- full_variables[grepl("log_likelihood|deviance|alpha_0|gamma|alpha|delta",full_variables)]

###################################
## Prepare stan data

mp_by_debate <- mp_by_debate[!(mp_by_debate$petition_debate == T & mp_by_debate$location == "Commons Chamber")]

mp_by_debate$nId <- as.numeric(as.factor(as.character(paste0(mp_by_debate$mId, mp_by_debate$new_data))))
mp_by_debate$dId_tmp <- as.numeric(as.factor(as.character(mp_by_debate$dId)))
mp_by_debate$mId_tmp <- as.numeric(as.factor(as.character(mp_by_debate$mId)))

setkey(mp_by_debate, nId, dId_tmp)

Xi_margin <- mp_by_debate[,unique(margin),by = nId]
setkey(Xi_margin, nId)
Xi_margin <- Xi_margin$V1/100

Xi_new_mp <- mp_by_debate[,unique(new_mp),by = nId]
setkey(Xi_new_mp, nId)
Xi_new_mp <- Xi_new_mp$V1

Xi_frontbench <- mp_by_debate[,any(minister|shadow_minister),by = nId]
setkey(Xi_frontbench, nId)
Xi_frontbench <- Xi_frontbench$V1

mp_parties_vec <- mp_by_debate[,unique(party),by = nId]
setkey(mp_parties_vec, nId)
mp_parties_vec <- mp_parties_vec$V1

mp_parties_vec <- factor(mp_parties_vec, levels = levels(mp_parties_vec)[c(1,3,4,2)])

Xi_party_matrix <- model.matrix(~mp_parties_vec-1)[,-1] # Conservative as the baseline category

P <- dim(Xi_party_matrix)[2]

Xd_petition <- mp_by_debate[,unique(petition_debate)[1],by = dId_tmp]
setkey(Xd_petition, dId_tmp)
Xd_petition <- Xd_petition$V1

mp_by_debate$location[is.na(mp_by_debate$location)] <- "Commons Chamber"

forStan = list(
  N_obs = nrow(mp_by_debate),
  K = length(topic_labels),
  N = length(unique(mp_by_debate$nId)),
  M = length(unique(mp_by_debate$mId)),
  D = length(unique(mp_by_debate$dId_tmp)),
  P = P,
  mId = mp_by_debate$mId_tmp,
  nId = mp_by_debate$nId,
  dId = mp_by_debate$dId_tmp,
  Y = mp_by_debate$N,
  X = as.matrix(debate_proportions[match(mp_by_debate$dId, debate_concat$dId),]),
  X_sig = as.numeric(scale(mp_by_debate$relative_sig_rate)),
  X_sig_commons = mp_by_debate$relative_sig_rate * as.numeric(mp_by_debate$location=="Commons Chamber"),
  X_sig_wminhall = mp_by_debate$relative_sig_rate * as.numeric(mp_by_debate$location!="Commons Chamber"),
  X_sig_constituency = mp_by_debate$relative_constituency_sig_rate,
  X_margin = mp_by_debate$margin,
  X_newmp = mp_by_debate$new_mp,
  X_sig_margin_interaction = mp_by_debate$relative_sig_rate * mp_by_debate$margin,
  Xi_margin = Xi_margin,
  Xi_new_mp = Xi_new_mp,
  Xi_cabinet = Xi_frontbench,
  Xi_party_matrix = Xi_party_matrix,
  Xd_petition = Xd_petition
)

labels_mid <- mp_by_debate[,unique(name),by = mId_tmp]
setkey(labels_mid, mId_tmp)
labels_mid <- labels_mid$V1

labels_dId <- mp_by_debate[,unique(debate_title),by = dId_tmp]
setkey(labels_dId, dId_tmp)
labels_dId <- labels_dId$V1

stanLabels <- list(
  labels_mId = labels_mid,
  labels_dId = labels_dId
)

###################################
## Estimate models

posterior_alpha_delta <- sampling(alpha_delta_stan_model,
                                    data = forStan,
                                    pars = alpha_delta_variables, 
                                    iter = HMCSample + HMCBurn,
                                    warmup = HMCBurn, 
                                    chains = HMCChains,
                                    cores = HMCChains)

variational_posterior_alpha_delta <- vb(alpha_delta_stan_model,
                                          data = forStan,
                                          pars = alpha_delta_variables, 
                                          iter = 15000, seed = 221186, 
                                          adapt_engaged = FALSE, eta  = 1,
                                          tol_rel_obj = 10e-6)  
  
save(variational_posterior_alpha_delta, posterior_alpha_delta, file= "working/stanOut/stanOut_alpha_delta_MCMC.Rdata")

print("FINISHED 05_model_run_MCMC.R")