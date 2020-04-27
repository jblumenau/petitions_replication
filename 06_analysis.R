##### ####################################################
#####                                               ######
#####           Analyse multiple stan models
#####                                               ######
##### ####################################################

# init ------------------------------------------------------------

rm(list=ls())

# Load libraries

library(loo) # CRAN v2.1.0
library(data.table) # CRAN v1.12.2
library(rstan) # CRAN v2.19.2
library(scales) # CRAN v1.0.0
library(stargazer) # CRAN v5.2.2
library(xtable) # CRAN v1.8-4
library(zoo) # CRAN v1.8-6 
library(mgcv) # CRAN v1.8-28 

## Load data

load("working/forStan.Rdata")
load("working/best_k.Rdata")
write(best_k, file = "latex/useful_numbers/best_k.tex")
load("working/mp_by_debate.Rdata")

## Load models

models <- c("simple", "alpha", "delta", "alpha_delta", paste0("full_",best_k))

for(m in models){
  
  load(paste0("working/stanOut/stanOut_",m,".Rdata"))    
  load(paste0("working/stanOut/dic_",m,".Rdata")) 

  }

## ##################
## Figure 1a

x <- mp_by_debate$relative_sig_rate[mp_by_debate$petition_debate & mp_by_debate$location != "Commons Chamber" ]
y <- mp_by_debate$N[mp_by_debate$petition_debate  & mp_by_debate$location != "Commons Chamber"]

gam_model <- gam(y ~ s(x, k=10), family = "binomial")
fitted_x_values <- seq(-1,20,0.1)
fitted_y_values <- predict(gam_model, newdata = data.frame(x = fitted_x_values), type = "response")

png("latex/figures/raw_data_participation.png", width = 6, height = 6, units = "in", res = 300)
cex.size <- 1.5
par(mar = c(5,5,2,2)+0.1, mfrow = c(1,1))
plot(fitted_x_values, fitted_y_values, ylim = c(0,1), type = "l", lwd = 5, xlab = "Relative signature rate", ylab = "Probability of participation", pch = 19, bty ="n", cex.axis = cex.size, cex.lab = cex.size,xaxt = "n")
points(x,y, pch = 19, col = alpha("gray",.2))
axis(1, at = seq(0,20,5), cex.axis = cex.size, cex.lab = cex.size)
dev.off()

## ##################
## Table 1 and table S9 -- Main participation effects

vars_for_table <- c("mu_gamma","mu_gamma_new_mp","mu_gamma_cabinet","mu_gamma_margin","mu_gamma_party","mu_alpha_new_mp","mu_alpha_cabinet","mu_alpha_margin","mu_alpha","mu_alpha_party","mu_delta_petition", "mu_delta")
posterior = variational_posterior_alpha_delta
vars = vars_for_table

extract_coef_intervals <- function(posterior, vars, model = ""){
  
  vars <- vars[vars%in%names(extract(posterior))]
  
  out <- lapply(vars, function(x){
      
    tmp <- extract(posterior, x)[[1]]
    
      if(length(dim(tmp))!=1){
        
        est <- apply(tmp,2, function(x) quantile(x,.5))
        hi <- apply(tmp,2, function(x) quantile(x,.975))
        lo <- apply(tmp,2, function(x) quantile(x,.025))
      
        x <- paste0(x,"_",gsub("mp_parties_vec","",colnames(forStan$Xi_party_matrix)))
        
        return(data.frame(vars = x, est,hi,lo))
        
      }else{
        
        est <- quantile(tmp,.5)
        hi <- quantile(tmp,.975)
        lo <- quantile(tmp,.025)
        
        return(data.frame(vars = x, est,hi,lo))
        
        
      }
      
  })  
  
  out <- data.frame(do.call("rbind", out))
  
  return(out)
  
}

full_coefs <- extract_coef_intervals(variational_posterior_full, vars_for_table, "full")
simple_coefs <- extract_coef_intervals(variational_posterior_simple, vars_for_table, "simple")
alpha_coefs <- extract_coef_intervals(variational_posterior_alpha, vars_for_table, "alpha")
delta_coefs <- extract_coef_intervals(variational_posterior_delta, vars_for_table, "delta")
alpha_delta_coefs <- extract_coef_intervals(variational_posterior_alpha_delta, vars_for_table, "alpha_delta")

create_cis <- function(coefs){
  coefs$print <- paste0("\\makecell{",round(coefs$est,2),"\\\\","(",round(coefs$lo,2),", ", round(coefs$hi,2),")}")
  return(coefs)
}

simple_coefs <- create_cis(simple_coefs)
alpha_coefs <- create_cis(alpha_coefs)
delta_coefs <- create_cis(delta_coefs)
alpha_delta_coefs <- create_cis(alpha_delta_coefs)
full_coefs <- create_cis(full_coefs)

coef_out <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "vars", all = TRUE),
       list(simple_coefs[,c("vars","print")],
            alpha_coefs[,c("vars","print")],
            delta_coefs[,c("vars","print")],
            alpha_delta_coefs[,c("vars","print")],
            full_coefs[,c("vars","print")]
            ))

names(coef_out) <- c("Variable", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5")

coef_out <- coef_out[match(c("mu_gamma_margin", "mu_gamma_cabinet", "mu_gamma_new_mp","mu_gamma_party_Labour","mu_gamma_party_LibDem","mu_gamma_party_Other","mu_gamma","mu_alpha_margin","mu_alpha_cabinet", "mu_alpha_new_mp","mu_alpha","mu_delta_petition","mu_delta"),coef_out$Variable),]

coef_out$Variable <- paste0("\\textbf{",c("$\\varphi_{\\text{Margin}}$", "$\\varphi_{\\text{Frontbench}}$","$\\varphi_{\\text{New MP}}$","$\\varphi_{\\text{Labour}}$","$\\varphi_{\\text{LibDem}}$","$\\varphi_{\\text{Other}}$","$\\varphi_{0}$","$\\mu_{\\text{Margin}}$","$\\mu_{\\text{Frontbench}}$","$\\mu_{\\text{New MP}}$","$\\mu_0$","$\\phi_{\\text{Petition}}$", "$\\phi_{0}$"),"}")

party_ticks <- c("$\\varphi$ Party dummies", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
party_ticks_two <- c("$\\mu$ Party dummies", "$\\times$", "$\\checkmark$", "$\\times$", "$\\checkmark$", "$\\checkmark$")
mp_ticks <- c("MP random effects", "$\\times$", "$\\checkmark$", "$\\times$", "$\\checkmark$", "$\\checkmark$")
debate_ticks <- c("Debate random effects", "$\\times$", "$\\times$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
topic_ticks <- c("MP-topic random effects", "$\\times$", "$\\times$", "$\\times$", "$\\times$", "$\\checkmark$")
N_obs_row <- c("N observations",rep(forStan$N_obs, dim(coef_out)[2]-1))
N_mps <- c("N MPs",rep(forStan$N, dim(coef_out)[2]-1))
N_debates <- c("N Debates",rep(forStan$D, dim(coef_out)[2]-1))

dic <- c("DIC",round(c(mean(dic_simple), mean(dic_alpha), mean(dic_delta), mean(dic_alpha_delta), mean(dic_full)),1))

N_parameters <- c("N parameters",c(length(names(variational_posterior_simple)), length(names(variational_posterior_alpha)), length(names(variational_posterior_delta)), length(names(variational_posterior_alpha_delta)), length(names(variational_posterior_full)))-1)

write(forStan$N_obs, file = "latex/useful_numbers/n_observations.tex")
write(forStan$N, file = "latex/useful_numbers/n_mps.tex")
write(forStan$D, file = "latex/useful_numbers/n_debates.tex")
write(sum(forStan$Xd_petition), file = "latex/useful_numbers/n_petition_debates.tex")

coef_out <- rbind(coef_out, party_ticks, party_ticks_two, mp_ticks, debate_ticks, topic_ticks, N_parameters, dic) #N_obs_row, N_mps, N_debates

alignment <- c("|c","|c","|c","|c","|c","|c|","c|")

to_print <- capture.output(print(xtable(coef_out, align = alignment, label = "tab:part_results"), sanitize.text.function = identity, include.rownames = F, caption.placement = "top",  scalebox = .90, hline.after = c(0,7,11,13, dim(coef_out)[1]), floating = F))

to_print[grep("Model",to_print)] <- "\\hline  & \\textbf{Model 1} & \\textbf{Model 2} & \\textbf{Model 3} & \\textbf{Model 4} & \\textbf{Model 5} \\\\ "


sink("latex/tables/multilevel_model_estimates.tex")
cat(paste(to_print, collapse = "\n"),"\n")
sink()

sink("latex/tables/multilevel_model_estimates_gamma_only.tex")
cat(paste(c(to_print[1:9],to_print[13],to_print[c(22,23,25:32)]), collapse = "\n"),"\n")
sink()

sink("latex/useful_numbers/baseline_participation_probability.txt")
cat(round(mean(forStan$Y)*100))
sink()


get_effect_interval <- function(x, var){

  effect <- round(100*(exp(x[x$vars == var,"est"])-1))
  effect_lo <- round(100*(exp(x[x$vars == var,"lo"])-1))
  effect_hi <- round(100*(exp(x[x$vars == var,"hi"])-1))
  
  out <- paste0(effect, "\\% [",effect_lo,"\\%, ", effect_hi,"\\%]")
  return(out)
}

sink("latex/useful_numbers/full_model_sig_odds_ratio_effect.txt")
cat(get_effect_interval(full_coefs, "mu_gamma"))
sink()

### ###########################
## Table S11 -- "no party" estimates

load(paste0("working/stanOut/robustness/stanOut_no_party.Rdata"))    

full_no_party_coefs <- extract_coef_intervals(variational_posterior_full_no_party, vars_for_table, "full_no_party")
full_no_party_coefs <- create_cis(full_no_party_coefs)

full_no_party_coefs <- full_no_party_coefs[match(c("mu_gamma_margin", "mu_gamma_cabinet", "mu_gamma_new_mp","mu_gamma","mu_alpha_margin","mu_alpha_cabinet", "mu_alpha_new_mp","mu_alpha","mu_delta_petition","mu_delta"),full_no_party_coefs$vars),]

full_no_party_coefs$Variable <- paste0("\\textbf{",c("$\\varphi_{\\text{Margin}}$", "$\\varphi_{\\text{Frontbench}}$","$\\varphi_{\\text{New MP}}$","$\\varphi_{0}$","$\\mu_{\\text{Margin}}$","$\\mu_{\\text{Frontbench}}$","$\\mu_{\\text{New MP}}$","$\\mu_0$","$\\phi_{\\text{Petition}}$", "$\\phi_{0}$"),"}")

mp_ticks <- c("MP random effects", "$\\checkmark$")
debate_ticks <- c("Debate random effects", "$\\checkmark$")
topic_ticks <- c("MP-topic random effects", "$\\checkmark$")
N_obs_row <- c("N observations", forStan$N_obs)
N_mps <- c("N MPs", forStan$N)
N_debates <- c("N Debates", forStan$D)

N_parameters <- c("N parameters",c(length(names(variational_posterior_full_no_party))))

coef_out <- rbind(full_no_party_coefs[,c("Variable","print")], mp_ticks, debate_ticks, topic_ticks, N_parameters) #N_obs_row, N_mps, N_debates
names(coef_out) <- c("", "\\textbf{Model 1}")
alignment <- c("|c|","|c|","c|")

to_print <- capture.output(print(xtable(coef_out, align = alignment, label = "tab:part_results"), sanitize.text.function = identity, include.rownames = F, caption.placement = "top",  scalebox = .95, hline.after = c(-1,0,4,8,10, dim(coef_out)[1]), floating = F))

sink("latex/tables/multilevel_model_estimates_no_party.tex")
cat(paste(to_print, collapse = "\n"),"\n")
sink()


#### ##########################
#### Figure 3a -- Second level effects

variational_posterior <- variational_posterior_full

alpha_0_chain <- extract(variational_posterior, "alpha_0")[[1]]
alpha_chain <- extract(variational_posterior, "alpha")[[1]]
delta_chain <- extract(variational_posterior, "delta")[[1]]
gamma_chain <- extract(variational_posterior, "gamma")[[1]]
beta_chain <- extract(variational_posterior, "Beta")[[1]]

alpha_0_chain <- extract(variational_posterior, "mu_alpha")[[1]]
alpha_new_mp_chain <- extract(variational_posterior, "mu_alpha_new_mp")[[1]]
alpha_cabinet_chain <- extract(variational_posterior, "mu_alpha_cabinet")[[1]]
alpha_margin_chain <- extract(variational_posterior, "mu_alpha_margin")[[1]]

gamma_0_chain <- extract(variational_posterior, "mu_gamma")[[1]]
gamma_new_mp_chain <- extract(variational_posterior, "mu_gamma_new_mp")[[1]]
gamma_cabinet_chain <- extract(variational_posterior, "mu_gamma_cabinet")[[1]]
gamma_margin_chain <- extract(variational_posterior, "mu_gamma_margin")[[1]]
gamma_labour_chain <- extract(variational_posterior, "mu_gamma_party")[[1]][,1]
gamma_ld_chain <- extract(variational_posterior, "mu_gamma_party")[[1]][,2]
gamma_other_chain <- extract(variational_posterior, "mu_gamma_party")[[1]][,3]

delta_0_chain <- extract(variational_posterior, "mu_delta")[[1]]
delta_petition_chain <- extract(variational_posterior, "mu_delta_petition")[[1]]

sigma_alpha_chain <- extract(variational_posterior, "sigma_alpha")[[1]]
sigma_delta_chain <- extract(variational_posterior, "sigma_delta")[[1]]
sigma_gamma_chain <- extract(variational_posterior, "sigma_gamma")[[1]]
sigma_beta_chain <- extract(variational_posterior, "sigma_beta")[[1]]

gamma_coefs <- data.frame(rbind(quantile((gamma_0_chain), c(0.025,0.05,0.5,0.95,0.975)),
                                quantile((gamma_cabinet_chain), c(0.025,0.05,0.5,0.95,0.975)),
                                quantile((gamma_new_mp_chain), c(0.025,0.05,0.5,0.95,0.975)),
                                quantile((gamma_margin_chain), c(0.025,0.05,0.5,0.95,0.975))))

gamma_coefs$variable <- c("Baseline", "Cabinet", "New MP", "Margin")

names(gamma_coefs) <- c("lo95","lo90","est","hi90", "hi95", "Variable")

gamma_coefs$id <- 1:dim(gamma_coefs)[1]

lims <- range(gamma_coefs[,1:5])*1.3

png("latex/figures/second_level_slope_coefficients.png",width = 7, height = 6, units = "in", res = 300)
par(mar = c(5,7,4,2)+0.1)
plot(gamma_coefs$est, gamma_coefs$id, xlim = lims, pch = 19, bty = "n", yaxt = "n", ylab = "", xlab = "Logit coefficient", cex = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
segments(x0 = gamma_coefs$lo90, x1= gamma_coefs$hi90, y0 = gamma_coefs$id, lwd = 4)
segments(x0 = gamma_coefs$lo95, x1= gamma_coefs$hi95, y0 = gamma_coefs$id, lwd = 2)
abline(v = 0)
abline(h = gamma_coefs$id, lty = 3, col = alpha("black",0.5))
axis(2, at = gamma_coefs$id, labels = c(expression(varphi["0"]),expression(varphi["Frontbench"]),expression(varphi["New MP"]),expression(varphi["Margin"])), las = 2, cex.lab = 1.5, cex.axis = 1.5)
dev.off()


#### ##########################
#### Figure 3b -- Marginal effect of one unit increase in signatures over the range of margin 

variational_posterior <- variational_posterior_full
gamma_0_chain <- extract(variational_posterior, "mu_gamma")[[1]]
gamma_margin_chain <- extract(variational_posterior, "mu_gamma_margin")[[1]]

margin_values <- seq(0,0.6,0.01)
fitted_chain <- sapply(margin_values, function(x) exp(gamma_0_chain + gamma_margin_chain*x))

ylims <- range(c(fitted_chain,0.8))

png("latex/figures/sig_rate_odds_ratio_interaction.png",width = 7, height = 6, units = "in", res = 300)
par(mar = c(5,5,4,2)+0.1)
plot(margin_values,fitted_chain[1,], col = "white", ylim = ylims, ylab = "Signature rate odds ratio", xlab = "2015 margin of victory", cex = 2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, bty = "n")
lines(margin_values, apply(fitted_chain,2,mean), lwd = 3)
abline(h = 1)
polygon(c(rev(margin_values), margin_values), c(rev(apply(fitted_chain,2,quantile,0.025)), apply(fitted_chain,2,quantile,0.975)), col = alpha('black',0.2), border = NA)
rug(forStan$Xi_margin)
dev.off()

#### ##########################
#### Predicted probability of debate participation over the range of signatures

variational_posterior <- variational_posterior_full

alpha_0_chain <- extract(variational_posterior, "alpha_0")[[1]]

mu_alpha_chain <- extract(variational_posterior,"mu_alpha")[[1]]

mu_delta_chain <- extract(variational_posterior,"mu_delta")[[1]]

mu_delta_petition_chain <- extract(variational_posterior,"mu_delta_petition")[[1]]

gamma_0_chain <- extract(variational_posterior, "mu_gamma")[[1]]
gamma_margin_chain <- extract(variational_posterior, "mu_gamma_margin")[[1]]
gamma_new_mp_chain <- extract(variational_posterior, "mu_gamma_new_mp")[[1]]

signature_values <- c(-.5,5)

linpred <- sapply(signature_values, function(x) alpha_0_chain + mu_alpha_chain + mu_delta_chain + alpha_new_mp_chain + mu_delta_petition_chain + (gamma_0_chain + gamma_new_mp_chain + (gamma_margin_chain * 0.01)) * x)

linpred_mean <- apply(linpred,2,mean)
linpred_lo <- apply(linpred,2,quantile,0.025)
linpred_hi <- apply(linpred,2,quantile,0.975)

linpred_uncomp <- sapply(signature_values, function(x) alpha_0_chain + mu_alpha_chain + mu_delta_chain + alpha_new_mp_chain + mu_delta_petition_chain + (gamma_0_chain  + (gamma_margin_chain * 0.4) ) * x)

linpred_mean_uncomp <- apply(linpred_uncomp,2,mean)
linpred_lo_uncomp <- apply(linpred_uncomp,2,quantile,0.025)
linpred_hi_uncomp <- apply(linpred_uncomp,2,quantile,0.975)

inv_logit <- function(x) exp(x)/(1+exp(x))

write(round(diff(inv_logit(linpred_mean))*100), file = "latex/useful_numbers/low_to_high_sig_competitive_probability_difference.tex")

### ######################################
### Table S5 -- Who participates on each topic?

k <- best_k

load(paste0("working/stanOut/stanOut_full_",k,".Rdata"))
load(paste0("working/topicmodel/ldaOut_",k,".Rdata"))

beta_chain <- extract(variational_posterior_full, "Beta")$Beta
beta_mean <- apply(beta_chain, c(2,3), mean)
beta_sd <- apply(beta_chain, c(2,3), sd)

# Eu and NHS

nhs_topic <- grep("nhs",topic_labels)[1]
eu_topic <- grep("eu",topic_labels)[1]
iraq_topic <- grep("iraq",topic_labels)[1]
school_topic <- grep("school",topic_labels)[1]

nhs_out <- head(stanLabels$labels_mId[order(beta_mean[,nhs_topic], decreasing = T)])
eu_out <- head(stanLabels$labels_mId[order(beta_mean[,eu_topic], decreasing = T)])
iraq_out <- head(stanLabels$labels_mId[order(beta_mean[,iraq_topic], decreasing = T)])
school_out <- head(stanLabels$labels_mId[order(beta_mean[,school_topic], decreasing = T)])

# Iraq and schools

eu_nhs <- cbind(eu_out, nhs_out)
iraq_schools <- cbind(iraq_out, school_out)

short_topic_labels <- sapply(1:length(topic_labels), function(x) substring(topic_labels[x],1,gregexpr("_", topic_labels)[x][[1]][5]-1))

combined_out<- data.frame(rbind(rbind(gsub("_", ";", short_topic_labels[c(eu_topic,nhs_topic)]), eu_nhs), rbind(gsub("_",";",short_topic_labels[c(iraq_topic,school_topic)]),iraq_schools)), stringsAsFactors = F)

names(combined_out) <- NULL

combined_out[1,] <- paste0("\\textbf{",combined_out[1,],"}")
combined_out[8,] <- paste0("\\textbf{",combined_out[8,],"}")

sink("latex/tables/combined_validation.tex")
print(xtable(combined_out, align = c("c","c","c")), hline.after = c(0,1,7,8), include.rownames = F, floating = F,sanitize.text.function = identity, scalebox = 0.85)
sink()

## ##############################
## Figure S3 -- Main effects for all models

stanout_files <- list.files("working/stanOut")
stanout_files <- stanout_files[grep("stanOut",stanout_files)]

gamma_out <- data.frame(matrix(NA, nrow = length(stanout_files), ncol = 4))
names(gamma_out) <- c("Model", "est", "hi", "lo")
gamma_out$Model <- gsub(".Rdata|stanOut_|full_","",stanout_files)
gamma_out$Model <- factor(gamma_out$Model, levels = c("simple","alpha","delta","alpha_delta","constituency",seq(5,100,5)))
gamma_margin_out <- gamma_frontbench_out <- gamma_new_out <- gamma_out

for(d in 1:nrow(gamma_out)) {
  
  load(paste0("working/stanOut/",stanout_files[d]))
  
  if(grepl("stanOut_alpha.Rdata", stanout_files[d])) variational_posterior <- variational_posterior_alpha
  if(grepl("stanOut_delta.Rdata", stanout_files[d])) variational_posterior <- variational_posterior_delta
  if(grepl("alpha_delta", stanout_files[d])) variational_posterior <- variational_posterior_alpha_delta
  if(grepl("simple", stanout_files[d])) variational_posterior <- variational_posterior_simple
  if(grepl("full", stanout_files[d])) variational_posterior <- variational_posterior_full
  if(grepl("stanOut_full_constituency.Rdata", stanout_files[d])) variational_posterior <- variational_posterior_full_constituency
  
  mu_gamma_chain <- extract(variational_posterior, "mu_gamma")[[1]]
  mu_gamma_margin_chain <- extract(variational_posterior, "mu_gamma_margin")[[1]]
  mu_gamma_cabinet_chain <- extract(variational_posterior, "mu_gamma_cabinet")[[1]]
  mu_gamma_new_mp_chain <- extract(variational_posterior, "mu_gamma_new_mp")[[1]]
  
  gamma_out[d,2] <- mean(mu_gamma_chain)
  gamma_out[d,3] <- quantile(mu_gamma_chain,0.975)
  gamma_out[d,4] <- quantile(mu_gamma_chain,0.025)
  
  gamma_margin_out[d,2] <- mean(mu_gamma_margin_chain)
  gamma_margin_out[d,3] <- quantile(mu_gamma_margin_chain,0.975)
  gamma_margin_out[d,4] <- quantile(mu_gamma_margin_chain,0.025)
  
  gamma_frontbench_out[d,2] <- mean(mu_gamma_cabinet_chain)
  gamma_frontbench_out[d,3] <- quantile(mu_gamma_cabinet_chain,0.975)
  gamma_frontbench_out[d,4] <- quantile(mu_gamma_cabinet_chain,0.025)
  
  gamma_new_out[d,2] <- mean(mu_gamma_new_mp_chain)
  gamma_new_out[d,3] <- quantile(mu_gamma_new_mp_chain,0.975)
  gamma_new_out[d,4] <- quantile(mu_gamma_new_mp_chain,0.025)
  
}

gamma_out <- gamma_out[gamma_out$Model%in%c(1:100),]
gamma_margin_out <- gamma_margin_out[gamma_margin_out$Model%in%c(1:100),]
gamma_frontbench_out <- gamma_frontbench_out[gamma_frontbench_out$Model%in%c(1:100),]
gamma_new_out <- gamma_new_out[gamma_new_out$Model%in%c(1:100),]

pdf("latex/figures/all_topic_model_gamma_coefficients.pdf",8,8)
par(mfrow = c(2,2))
plot(as.numeric(gamma_out$Model), gamma_out$est, ylim = c(-1,1), xaxt = "n", ylab = "Estimate", xlab = "Topic Model", main = expression(varphi[0]), pch = 19,col = ifelse(gamma_new_out$Model == best_k,"red","black"))
axis(1, at = as.numeric(gamma_out$Model), gamma_out$Model)
segments(as.numeric(gamma_out$Model), y0 = gamma_out$lo, y1 = gamma_out$hi, col = ifelse(gamma_new_out$Model == best_k,"red","black"))
abline(h = 0)

plot(as.numeric(gamma_margin_out$Model), gamma_margin_out$est, ylim = c(-1,1), xaxt = "n", ylab = "Estimate", xlab = "Topic Model", main = expression(varphi["Margin"]), pch = 19, col = ifelse(gamma_new_out$Model == best_k,"red","black"))
axis(1, at = as.numeric(gamma_out$Model), gamma_out$Model)
segments(as.numeric(gamma_margin_out$Model), y0 = gamma_margin_out$lo, y1 = gamma_margin_out$hi, col = ifelse(gamma_new_out$Model == best_k,"red","black"))
abline(h = 0)

plot(as.numeric(gamma_frontbench_out$Model), gamma_frontbench_out$est, ylim = c(-1,1), xaxt = "n", ylab = "Estimate", xlab = "Topic Model", main = expression(varphi["Frontbench"]), pch = 19, col = ifelse(gamma_new_out$Model == best_k,"red","black"))
axis(1, at = as.numeric(gamma_out$Model), gamma_out$Model)
segments(as.numeric(gamma_frontbench_out$Model), y0 = gamma_frontbench_out$lo, y1 = gamma_frontbench_out$hi, col = ifelse(gamma_new_out$Model == best_k,"red","black"))
abline(h = 0)

plot(as.numeric(gamma_new_out$Model), gamma_new_out$est, ylim = c(-1,1), xaxt = "n", ylab = "Estimate", xlab = "Topic Model", main = expression(varphi["New MP"]), pch = 19, col = ifelse(gamma_new_out$Model == best_k,"red","black"))
axis(1, at = as.numeric(gamma_out$Model), gamma_out$Model)
segments(as.numeric(gamma_new_out$Model), y0 = gamma_new_out$lo, y1 = gamma_new_out$hi, col = ifelse(gamma_new_out$Model == best_k,"red","black"))
abline(h = 0)
dev.off()

## #################################
## Table S8 -- Robustness to independent variable specification

mp_by_debate$sig_rate2 <- 0
mp_by_debate$sig_rate2[mp_by_debate$petition_debate] <- (mp_by_debate$signature_count[mp_by_debate$petition_debate]+1)/mp_by_debate$Total2015[mp_by_debate$petition_debate]
mp_by_debate$log_rel_sig_rate <- 0
mp_by_debate$log_rel_sig_rate[mp_by_debate$petition_debate] <- log(mp_by_debate$sig_rate2[mp_by_debate$petition_debate]/mp_by_debate$national_sig_rate[mp_by_debate$petition_debate])

mp_by_debate$tmp <- mp_by_debate$relative_sig_rate
mp_by_debate$tmp[mp_by_debate$petition_debate] <- (mp_by_debate$tmp[mp_by_debate$petition_debate] - mean(mp_by_debate$tmp[mp_by_debate$petition_debate]))/sd(mp_by_debate$tmp[mp_by_debate$petition_debate])
baseline_model <- glm(N ~ tmp, data = mp_by_debate, family = "binomial", subset = mp_by_debate$petition_debate & mp_by_debate$location != "Commons Chamber")

mp_by_debate$tmp <- log(mp_by_debate$signature_count+1)
mp_by_debate$tmp[mp_by_debate$petition_debate] <- (mp_by_debate$tmp[mp_by_debate$petition_debate] - mean(mp_by_debate$tmp[mp_by_debate$petition_debate]))/sd(mp_by_debate$tmp[mp_by_debate$petition_debate])
sig_count_model <- glm(N ~ tmp, data = mp_by_debate, family = "binomial", subset = mp_by_debate$petition_debate & mp_by_debate$location != "Commons Chamber")


mp_by_debate$tmp <- mp_by_debate$sig_rate
mp_by_debate$tmp[mp_by_debate$petition_debate] <- (mp_by_debate$tmp[mp_by_debate$petition_debate] - mean(mp_by_debate$tmp[mp_by_debate$petition_debate]))/sd(mp_by_debate$tmp[mp_by_debate$petition_debate])
sig_rate_model <- glm(N ~ tmp, data = mp_by_debate, family = "binomial", subset = mp_by_debate$petition_debate & mp_by_debate$location != "Commons Chamber")


mp_by_debate$tmp <- mp_by_debate$relative_constituency_sig_rate
mp_by_debate$tmp[mp_by_debate$petition_debate] <- (mp_by_debate$tmp[mp_by_debate$petition_debate] - mean(mp_by_debate$tmp[mp_by_debate$petition_debate]))/sd(mp_by_debate$tmp[mp_by_debate$petition_debate])
constituency_rel_sig_model <- glm(N ~ tmp, data = mp_by_debate, family = "binomial", subset = mp_by_debate$petition_debate & mp_by_debate$location != "Commons Chamber")


mp_by_debate$tmp <- mp_by_debate$log_rel_sig_rate
mp_by_debate$tmp[mp_by_debate$petition_debate] <- (mp_by_debate$tmp[mp_by_debate$petition_debate] - mean(mp_by_debate$tmp[mp_by_debate$petition_debate]))/sd(mp_by_debate$tmp[mp_by_debate$petition_debate])
log_rel_sig_model <- glm(N ~ tmp, data = mp_by_debate, family = "binomial", subset = mp_by_debate$petition_debate & mp_by_debate$location != "Commons Chamber")

sink("latex/tables/signature_spec_comparison.tex")
stargazer(baseline_model, log_rel_sig_model, sig_count_model, sig_rate_model, constituency_rel_sig_model, 
          no.space = T, keep.stat = c("n","AIC"),
          covariate.labels = "Signature Variable",
          dep.var.caption = "",
          dep.var.labels = "Participation",
          title = "Bivariate logit -- Participation",
          label = "tab:x_spec_comparison")
sink()


#### ##########################
#### Figure S5 -- Second level effects double gamma model

load("working/stanOut/robustness/stanOut_double_gamma.Rdata")

variational_posterior <- variational_posterior_full_double_gamma

alpha_0_chain <- extract(variational_posterior, "mu_alpha")[[1]]
alpha_new_mp_chain <- extract(variational_posterior, "mu_alpha_new_mp")[[1]]
alpha_cabinet_chain <- extract(variational_posterior, "mu_alpha_cabinet")[[1]]
alpha_margin_chain <- extract(variational_posterior, "mu_alpha_margin")[[1]]

gamma_0_chain <- extract(variational_posterior, "mu_gamma")[[1]]
gamma_new_mp_chain <- extract(variational_posterior, "mu_gamma_new_mp")[[1]]
gamma_cabinet_chain <- extract(variational_posterior, "mu_gamma_cabinet")[[1]]
gamma_margin_chain <- extract(variational_posterior, "mu_gamma_margin")[[1]]

delta_0_chain <- extract(variational_posterior, "mu_delta")[[1]]
delta_petition_chain <- extract(variational_posterior, "mu_delta_petition")[[1]]

sigma_alpha_chain <- extract(variational_posterior, "sigma_alpha")[[1]]
sigma_delta_chain <- extract(variational_posterior, "sigma_delta")[[1]]
sigma_beta_chain <- extract(variational_posterior, "sigma_beta")[[1]]

int_func <- function(x) quantile(x, c(0.025,0.05,0.5,0.9,0.975))

gamma_coefs_commons <- data.frame(rbind(
  int_func(gamma_0_chain[,1]),
  int_func(gamma_cabinet_chain[,1]),
  int_func(gamma_new_mp_chain[,1]),
  int_func(gamma_margin_chain[,1])))

gamma_coefs_wmin <- data.frame(rbind(
  int_func(gamma_0_chain[,2]),
  int_func(gamma_cabinet_chain[,2]),
  int_func(gamma_new_mp_chain[,2]),
  int_func(gamma_margin_chain[,2])))


gamma_coefs_commons$variable <- c("Intercept", "Cabinet", "New MP", "Margin")

names(gamma_coefs_commons) <- c("lo","lo90","est", "hi90", "hi", "Variable")

gamma_coefs_commons$id <- (1:dim(gamma_coefs_commons)[1])+.1

gamma_coefs_wmin$variable <- c("Intercept", "Cabinet", "New MP", "Margin")

names(gamma_coefs_wmin) <- c("lo","lo90","est", "hi90", "hi", "Variable")

gamma_coefs_wmin$id <- (1:dim(gamma_coefs_wmin)[1])-.1

xlims <- range(c(gamma_coefs_wmin[,1:5], gamma_coefs_commons[,1:5]))*1.3

pdf("latex/figures/second_level_slope_coefficients_double.pdf",6,6)
par(mar = c(5,7,0,2)+0.1)
cex.size <- 2.5
plot(gamma_coefs_wmin$est, gamma_coefs_wmin$id, xlim = xlims*1.2, ylim = c(.5,5.2), pch = 19, bty = "n", yaxt = "n", ylab = "", xlab = "Logit coefficient", cex = cex.size, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, col = alpha("blue",1))
points(gamma_coefs_commons$est, gamma_coefs_commons$id, pch = 19, col = alpha("black",1), cex = cex.size)
segments(x0 = gamma_coefs_wmin$lo90, x1= gamma_coefs_wmin$hi90, y0 = gamma_coefs_wmin$id, lwd = cex.size*1.5, col = alpha("blue",1))
segments(x0 = gamma_coefs_wmin$lo, x1= gamma_coefs_wmin$hi, y0 = gamma_coefs_wmin$id, lwd = cex.size, col = alpha("blue",1))
segments(x0 = gamma_coefs_commons$lo, x1= gamma_coefs_commons$hi, y0 = gamma_coefs_commons$id, lwd = cex.size, col = alpha("black",1))
segments(x0 = gamma_coefs_commons$lo90, x1= gamma_coefs_commons$hi90, y0 = gamma_coefs_commons$id, lwd = cex.size*1.5, col = alpha("black",1))
#abline(v = 0)
segments(x0 = 0, y0 = 0, y1 = 4.5, lwd = 2)
abline(h = gamma_coefs_wmin$id+.1, lty = 3, col = alpha("black",0.5))
axis(2, at = gamma_coefs_wmin$id+.1, labels = c(expression(varphi["0"]),expression(varphi["Government"]),expression(varphi["New MP"]),expression(varphi["Margin"])), las = 2, cex.lab = 1.5, cex.axis = 1.5)
legend("topright", legend = c("Commons", "Westminster Hall"), lty = 1, cex = 1.3, lwd = 3, col = c("black","blue"), bty = "n", xpd = F)
dev.off()

## ############### 
## Figure S6 -- MCMC comparison

## Correlation between second-level variables

tmp <- data.frame(margin = forStan$Xi_margin,
           frontbench = forStan$Xi_cabinet,
           new_mp = forStan$Xi_new_mp,
           party1 = forStan$Xi_party_matrix[,1],
           party2 = forStan$Xi_party_matrix[,2],
           party3 = forStan$Xi_party_matrix[,3])
tmp <- cor(tmp)
diag(tmp) <- 0
max((tmp))

## Compare MCMC to variational inference for alpha-delta model

load("working/stanOut/stanOut_alpha_delta_MCMC.Rdata") # Loads the MCMC and variational inference with modified sig_rate variable to improve MCMC efficiency

variational_params <- extract(variational_posterior_alpha_delta)
mcmc_params <- extract(posterior_alpha_delta)

ci_func <- function(x = "alpha_0", params = variational_params, model_name = "vi"){
  
  est <- mean(params[[x]])
  hi <- quantile(params[[x]], .975)
  lo <- quantile(params[[x]], .025)
  return(data.frame(model = model_name,var = x, est, hi, lo))
}

ci_func <- function(x, model_name = "vi", var){
  
  est <- as.numeric(mean(x))
  hi <- as.numeric(quantile(x, .975))
  lo <- as.numeric(quantile(x, .025))
  return(data.frame(model = model_name, var, est, hi, lo))
}
x <- variational_params[["mu_gamma"]]

vars <-c("mu_gamma", "mu_gamma_new_mp", "mu_gamma_cabinet", "mu_gamma_margin") 

gamma_est_vi <- do.call("rbind",lapply(vars, function(x) ci_func(variational_params[[x]], "vi", x)))

gamma_est_mcmc <- do.call("rbind",lapply(vars, function(x) ci_func(mcmc_params[[x]], "mcmc", x)))

gamma_party_est_vi <- do.call("rbind",lapply(1:3, function(i) ci_func(variational_params$mu_gamma_party[,i],"vi",paste0("party_",i))))

gamma_party_est_mcmc <- do.call("rbind",lapply(1:3, function(i) ci_func(mcmc_params$mu_gamma_party[,i],"mcmc",paste0("party_",i))))

gamma_ests <- rbind(gamma_est_vi, gamma_party_est_vi, gamma_est_mcmc, gamma_party_est_mcmc)

gamma_ests$width <- gamma_ests$hi - gamma_ests$lo
gamma_ests$width_ratio <- NA
gamma_ests$width_ratio[gamma_ests$model == "vi"] <- gamma_ests$width[gamma_ests$model == "vi"]/gamma_ests$width[gamma_ests$model == "mcmc"]


# Apply adjustment to full model estimates

load(paste0("working/stanOut/stanOut_",paste0("full_",best_k),".Rdata"))    

variational_posterior <- variational_posterior_full

gamma_0_chain <- extract(variational_posterior, "mu_gamma")[[1]]
gamma_new_mp_chain <- extract(variational_posterior, "mu_gamma_new_mp")[[1]]
gamma_cabinet_chain <- extract(variational_posterior, "mu_gamma_cabinet")[[1]]
gamma_margin_chain <- extract(variational_posterior, "mu_gamma_margin")[[1]]
gamma_labour_chain <- extract(variational_posterior, "mu_gamma_party")[[1]][,1]
gamma_ld_chain <- extract(variational_posterior, "mu_gamma_party")[[1]][,2]
gamma_other_chain <- extract(variational_posterior, "mu_gamma_party")[[1]][,3]

gamma_coefs <- data.frame(rbind(quantile((gamma_0_chain), c(0.025,0.05,0.5,0.95,0.975)),
                                quantile((gamma_cabinet_chain), c(0.025,0.05,0.5,0.95,0.975)),
                                quantile((gamma_new_mp_chain), c(0.025,0.05,0.5,0.95,0.975)),
                                quantile((gamma_margin_chain), c(0.025,0.05,0.5,0.95,0.975)),
                                quantile((gamma_labour_chain), c(0.025,0.05,0.5,0.95,0.975)),
                                quantile((gamma_ld_chain), c(0.025,0.05,0.5,0.95,0.975)),
                                quantile((gamma_other_chain), c(0.025,0.05,0.5,0.95,0.975))))

gamma_coefs$width <- gamma_coefs$X97.5. - gamma_coefs$X2.5.


gamma_coefs$adjusted_width <- gamma_coefs$width * data.table(gamma_ests)[,width[model == "mcmc"]/width[model == "vi"],by = var]$V1
#gamma_coefs$adjusted_width <- gamma_coefs$width * 2#mean(data.table(gamma_ests)[,width[model == "mcmc"]/width[model == "vi"],by = var]$V1)

gamma_coefs$hi_adj <- gamma_coefs$X50. + (gamma_coefs$adjusted_width/2)
gamma_coefs$lo_adj <- gamma_coefs$X50. - (gamma_coefs$adjusted_width/2)

gamma_coefs$var <- c("gamma_0", "gamma_cabinet", "gamma_new_mp", "gamma_margin", "party_1", "party_2","party_3")

names(gamma_coefs) <- c("lo","lo5","est","hi95","hi","width", "adj_width","hi_adj","lo_adj", "var")

var_labels = c("mu_gamma" = expression(varphi["0"]),
           "mu_gamma_margin" = expression(varphi["Margin"]),
           "mu_gamma_new_mp" = expression(varphi["New MP"]),
           "mu_gamma_cabinet" = expression(varphi["Frontbench"]),
           "party_1" = expression(varphi["Labour"]),
           "party_2" = expression(varphi["LibDem"]),
           "party_3" = expression(varphi["Other"]))

average_interval_width_ratio <- mean(gamma_ests$width[gamma_ests$model == "vi"]/gamma_ests$width[gamma_ests$model == "mcmc"])

write(round(average_interval_width_ratio*100), file = "latex/useful_numbers/vi_percent_of_mcmc_width.txt")

pdf("latex/figures/vi_mcmc_comparison_plot.pdf",12,6)
par(oma = c(0,2,0,0), mfrow = c(1,2))

plot(1:7, gamma_ests$width_ratio[1:7], ylim = c(0,1.1), xlab = "", xaxt = "n", ylab = "Ratio of VI 95% interval to HMC 95% interval", pch = 19, bty = "n", main = "Comparing VI and HMC 95% credible intervals")
abline(h = 1, lty = 3)
axis(1, at = 1:7, labels = var_labels)

plot(gamma_coefs$est, 1:length(gamma_coefs$est), 
     xlim = range(c(gamma_coefs$lo, gamma_coefs$hi, gamma_coefs$lo_adj, gamma_coefs$hi_adj))*1.1, 
     pch = 19,
     xlab = "Estimate",
     ylab = "", 
     yaxt = "n",
     bty = "n",
     main = "Full model adjusted intervals")
legend("topright", legend = c("VI", "HMC"), lwd = c(3,1), bty = "n")
axis(2, at = 1:7, labels =var_labels, las = 2)
segments(x0 = gamma_coefs$lo,
         x1 = gamma_coefs$hi,
         y0 = 1:length(gamma_coefs$lo), lwd = 3)
segments(x0 = gamma_coefs$lo_adj,
         x1 = gamma_coefs$hi_adj,
         y0 = 1:length(gamma_coefs$lo_adj), lwd = 1)
abline(v = 0, lty = 3)
dev.off()


#### ######################################
## Figure S4; Conditional-treatment-variance weights

mp_by_debate <- mp_by_debate[!(mp_by_debate$petition_debate & mp_by_debate$location == "Commons Chamber") ,]

tmp <- mp_by_debate[petition_debate == T,list(var_sig_rate =var(sig_rate),
                                       var_relative_sig_rate = var(relative_sig_rate),
                                       var_relative_constituency_sig_rate = var(relative_constituency_sig_rate),
                                       signature_count = sum(signature_count)),by = dId]


pdf("latex/figures/conditional_variance_sig_rate.pdf",8,4)
par(mfrow = c(1,2))
plot(tmp$signature_count, tmp$var_relative_sig_rate, xlab = "Total signature count", ylab = "Variance of equation S6", main = "Conditional variance of equation S6 \n by total signature count", pch = 19, col = alpha("black",.2))
plot(tmp$signature_count, tmp$var_sig_rate, xlab = "Total signature count", ylab = "Variance of equation S7", main = "Conditional variance of equation S7 \n by total signature count", pch = 19, col = alpha("black",.2))
#plot(tmp$signature_count, tmp$var_relative_constituency_sig_rate, xlab = "Total signature count", ylab = "Variance of equation S8", main = "Conditional variance of equation S8 \n by total signature count", pch = 19, col = alpha("black",.2))
dev.off()


print("FINISHED 06_analysis.R")