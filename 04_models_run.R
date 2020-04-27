##### ####################################################
#####                                               ######
#####              Estimate stan models
#####                                               ######
##### ####################################################

# init ------------------------------------------------------------

rm(list=ls())

# Load libraries

library(data.table) # CRAN v1.12.2
library(rstan) # CRAN v2.19.2
library(loo) # CRAN v2.1.0

adapt_engaged <- FALSE
runSimple <- TRUE
runAlpha <- TRUE
runDelta <- TRUE
runAlphaDelta <- TRUE
runFull <- TRUE
calculateDIC <- TRUE
runConstituencyFull <- TRUE
runNoParty <- TRUE
runDoubleGamma <- TRUE

## Load data

load("working/mp_by_debate.Rdata")
load("working/topicmodel/ldaOut_10.Rdata")
load("working/saved_stan_models.Rdata")
load("working/text_objects.Rdata")

Ks <- sort(as.numeric(gsub("ldaOut_|\\.Rdata","",list.files("working/topicmodel"))))

## Specify models

full_variables <- c("alpha", "delta", "gamma", "alpha_0", "Beta",
                    "mu_alpha","mu_alpha_new_mp", "mu_alpha_cabinet","mu_alpha_margin","mu_alpha_party",
                    "mu_gamma", "mu_gamma_new_mp", "mu_gamma_cabinet","mu_gamma_margin","mu_gamma_party",
                    "mu_delta", "mu_delta_petition",
                    "sigma_alpha", "sigma_delta", "sigma_gamma", "sigma_beta", "deviance", "log_likelihood")

simple_variables <- full_variables[grepl("log_likelihood|deviance|alpha_0|gamma",full_variables)]

alpha_variables <- full_variables[grepl("log_likelihood|deviance|alpha_0|gamma|alpha",full_variables)]

delta_variables <- full_variables[grepl("log_likelihood|deviance|alpha_0|gamma|delta",full_variables)]

alpha_delta_variables <- full_variables[grepl("log_likelihood|deviance|alpha_0|gamma|alpha|delta",full_variables)]

full_double_gamma_variables <- c("alpha_gamma_individual","sigma_gamma_commons","sigma_gamma_wminhall",full_variables[!full_variables%in%c("alpha","gamma","sigma_gamma")])

linear_variables <- c(full_variables[!full_variables%in%c("deviance","log_likelihood")],"sigma_y")

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
  X_sig = mp_by_debate$relative_sig_rate,
  X_sig_not_relative = mp_by_debate$sig_rate,
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

save(forStan, stanLabels, file = "working/forStan.Rdata")

# Estimate models and calculate approximate AIC

if(runSimple){
  
  variational_posterior_simple <- vb(
    object = simple_stan_model, 
    data = forStan,
    pars = simple_variables,
    iter = 10000, seed = 2118, adapt_engaged = adapt_engaged, eta  = 1)
  
  print(variational_posterior_simple, pars = c("mu_gamma", "mu_gamma_new_mp", "mu_gamma_cabinet", "mu_gamma_margin"))
  
  var_params <- extract(variational_posterior_simple)
  
  deviance_chain <- extract(variational_posterior_simple, "deviance")[[1]]
  
  dic_simple <- .5*var(deviance_chain) + mean(deviance_chain)
  
  save(variational_posterior_simple, file= "working/stanOut/stanOut_simple.Rdata")
  save(dic_simple, file= "working/stanOut/dic_simple.Rdata")
  
}

if(runAlpha){
  
  variational_posterior_alpha <- vb(
    object = alpha_stan_model, 
    data = forStan,
    pars = alpha_variables,
    iter = 10000, seed = 2118, adapt_engaged = adapt_engaged, eta  = 1)
  
  deviance_chain <- extract(variational_posterior_alpha, "deviance")[[1]]
  
  dic_alpha <- .5*var(deviance_chain) + mean(deviance_chain)
  
  save(variational_posterior_alpha, file= "working/stanOut/stanOut_alpha.Rdata")
  save(dic_alpha, file= "working/stanOut/dic_alpha.Rdata")
  
}

if(runDelta){
  
  variational_posterior_delta <- vb(
    object = delta_stan_model, 
    data = forStan,
    pars = delta_variables,
    iter = 10000, seed = 2118, adapt_engaged = adapt_engaged, eta  = 1)
  
  deviance_chain <- extract(variational_posterior_delta, "deviance")[[1]]
  
  dic_delta <- .5*var(deviance_chain) + mean(deviance_chain)
  
  save(variational_posterior_delta, file= "working/stanOut/stanOut_delta.Rdata")
  save(dic_delta, file= "working/stanOut/dic_delta.Rdata")
  
}

if(runAlphaDelta){
  
  forStan$X_sig <- mp_by_debate$relative_sig_rate
  
  variational_posterior_alpha_delta <- vb(
    object = alpha_delta_stan_model, 
    data = forStan,
    pars = alpha_delta_variables,
    iter = 10000, seed = 221186, adapt_engaged = adapt_engaged, eta  = 1)
  
  deviance_chain <- extract(variational_posterior_alpha_delta, "deviance")[[1]]
  
  dic_alpha_delta <- .5*var(deviance_chain) + mean(deviance_chain)
  
  save(variational_posterior_alpha_delta, file= "working/stanOut/stanOut_alpha_delta.Rdata")
  save(dic_alpha_delta, file= "working/stanOut/dic_alpha_delta.Rdata")
  
}

if(runFull){
  
  for(k in Ks){
    
  load(paste0("working/topicmodel/ldaOut_",k,".Rdata"))
  
  forStan$X <- as.matrix(debate_proportions[match(mp_by_debate$dId, debate_concat$dId),])
  forStan$K <- length(topic_labels)
  
  variational_posterior_full <- vb(
    object = full_stan_model, 
    data = forStan,
    pars = full_variables,
    iter = 10000, seed = 2118, adapt_engaged = adapt_engaged, eta  = 1)
  
  deviance_chain <- extract(variational_posterior_full, "deviance")[[1]]
  
  dic_full <- .5*var(deviance_chain) + mean(deviance_chain)
  
  save(variational_posterior_full, file= paste0("working/stanOut/stanOut_full_",k,".Rdata"))
  save(dic_full, file= paste0("working/stanOut/dic_full_",k,".Rdata"))
  
  }
  
}

if(calculateDIC){
  
  dic_files <- list.files("working/stanOut")
  dic_files <- dic_files[grep("stanOut",dic_files)]
  dic_files <- dic_files[!grepl("MCMC|constituency",dic_files)]
  d <- 1
  dic_out <- data.frame(matrix(NA, nrow = length(dic_files), ncol = 2))
  names(dic_out) <- c("Model", "DIC")
  for(d in 1:nrow(dic_out)) {
    print(dic_files[d])
    load(paste0("working/stanOut/",dic_files[d]))
    
    if(grepl("stanOut_alpha.Rdata",dic_files[d])) variational_posterior <- variational_posterior_alpha
    if(grepl("stanOut_delta.Rdata",dic_files[d])) variational_posterior <- variational_posterior_delta
    if(grepl("alpha_delta",dic_files[d])) variational_posterior <- variational_posterior_alpha_delta
    if(grepl("simple",dic_files[d])) variational_posterior <- variational_posterior_simple
    if(grepl("full",dic_files[d])) variational_posterior <- variational_posterior_full
    
    deviance_chain <- extract(variational_posterior, "deviance")[[1]]
    
    dic_out$DIC[d] <- (.5*var(deviance_chain)) + mean(deviance_chain) # http://www.stat.columbia.edu/~gelman/research/published/waic_understand3.pdf equation 10
    dic_out$Model[d] <- dic_files[d]
    
    
  }
  
  dic_out$Model <- gsub("stanOut_|.Rdata","",dic_out$Model)
  dic_out$Model <- droplevels(factor(dic_out$Model, levels = c("simple","delta","alpha", "alpha_delta",paste0("full_",1:100))))
  
  best_k <- as.numeric(gsub("full_","", as.character(dic_out[which.min(dic_out$DIC),]$Model)))
  save(best_k, file = "working/best_k.Rdata")
  
  dic_out <- dic_out[grep("full|alpha_delta", dic_out$Model),]
  levels(dic_out$Model)[levels(dic_out$Model)=="alpha_delta"] <- "Null"
  
  pdf("latex/figures/dic.pdf",6,5)
  par(mar = c(5,4,4,2)+0.1)
  plot(as.numeric(dic_out$Model), dic_out$DIC, xaxt = "n", pch = 19, xlab = "Topic model", ylab = "DIC")
  axis(1, at = as.numeric(dic_out$Model), gsub("full_","",as.character(dic_out$Model)), las=  2)
  abline(v = as.numeric(dic_out$Model), lty = 3, col = "gray")
  dev.off()
  
  
}


if(runConstituencyFull){
  
  load("working/best_k.Rdata")
  k <- best_k
  load(paste0("working/topicmodel/ldaOut_",k,".Rdata"))
  
  forStan$X <- as.matrix(debate_proportions[match(mp_by_debate$dId, debate_concat$dId),])
  forStan$K <- length(topic_labels)
  forStan$X_sig <- forStan$X_sig_constituency
  
  variational_posterior_full_constituency <- vb(
    object = full_stan_model, 
    data = forStan,
    pars = full_variables,
    iter = 10000, seed = 2118, adapt_engaged = adapt_engaged, eta  = 1)
  
  save(variational_posterior_full_constituency, file= paste0("working/stanOut/stanOut_full_constituency.Rdata"))
   
}

if(runNoParty){
  
  load("working/best_k.Rdata")
  k <- best_k
  load(paste0("working/topicmodel/ldaOut_",k,".Rdata"))
  
  forStan$X <- as.matrix(debate_proportions[match(mp_by_debate$dId, debate_concat$dId),])
  forStan$K <- length(topic_labels)
  forStan$X_sig <- forStan$X_sig_constituency
  
  variational_posterior_full_no_party <- vb(
    object = full_stan_model_no_party, 
    data = forStan,
    pars = full_variables[!grepl("party",full_variables)],
    iter = 10000, seed = 2118, adapt_engaged = adapt_engaged, eta  = 1)
  
  save(variational_posterior_full_no_party, file= paste0("working/stanOut/robustness/stanOut_no_party.Rdata"))
  
}

if(runDoubleGamma){
    
    load("working/mp_by_debate.Rdata")
    load("working/saved_stan_models.Rdata")
    load("working/text_objects.Rdata")
    k <- 45
    
    load(paste0("working/topicmodel/ldaOut_",k,".Rdata"))
    
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
      X_sig = mp_by_debate$relative_sig_rate,
      X_sig_commons = mp_by_debate$relative_sig_rate * as.numeric(mp_by_debate$location=="Commons Chamber"),
      X_sig_wminhall = mp_by_debate$relative_sig_rate * as.numeric(mp_by_debate$location!="Commons Chamber"),
      X_sig_constituency = mp_by_debate$relative_constituency_sig_rate,
      X_margin = mp_by_debate$margin,
      X_newmp = mp_by_debate$new_mp,
      X_sig_margin_interaction = mp_by_debate$relative_sig_rate * mp_by_debate$margin,
      Xi_margin = Xi_margin,
      Xi_new_mp = Xi_new_mp,
      Xi_cabinet = Xi_frontbench, # Changed definition -- all those who have held a government/shadow minister post of cabinet level during either term
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
    
    forStan$X <- as.matrix(debate_proportions[match(mp_by_debate$dId, debate_concat$dId),])
    forStan$K <- length(topic_labels)
    
    forStan$sigma_prior <- diag(3)*4
    forStan$sigma_dim <- nrow(forStan$sigma_prior)
    
    variational_posterior_full_double_gamma <- vb(
      object = full_stan_model_double_gamma, 
      data = forStan,
      pars = full_double_gamma_variables,
      iter = 10000, seed = 2118, adapt_engaged = adapt_engaged, eta  = 1)
    
    deviance_chain <- extract(variational_posterior_full_double_gamma, "deviance")[[1]]
    
    dic_double_gamma <- .5*var(deviance_chain) + mean(deviance_chain)
    
    save(dic_double_gamma, file= "working/stanOut/robustness/dic_full.Rdata")
    save(variational_posterior_full_double_gamma, file= "working/stanOut/robustness/stanOut_double_gamma.Rdata")
    
}


print("FINISHED 04_model_run.R")