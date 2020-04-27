##### ####################################################
#####                                               ######
#####           Analyse multiple stan models
#####                                               ######
##### ####################################################

# init ------------------------------------------------------------

rm(list=ls())

# Load libraries

library(data.table) # CRAN v1.12.2
library(rstan) # CRAN v2.19.2
library(stargazer) # CRAN v5.2.2
library(mgcv) # CRAN v1.8-28 
library(xtable) # CRAN v1.8-4 

## Load data

load("working/forStan_approval.Rdata")
for(i in c("simple","alpha","delta","full")){
  load(paste0("working/stanOut/approval/stanOut_",i,".Rdata"))
  load(paste0("working/stanOut/approval/dic_",i,".Rdata"))
}

## Descriptive plot

x <- mp_by_pet_debate$relative_sig_rate
y <- mp_by_pet_debate$approve

# Remove outliers

gam_model <- gam(y ~ s(x, k=7), family = "binomial")
fitted_x_values <- seq(-1,20,0.01)
fitted_y_values <- predict(gam_model, newdata = data.frame(x = fitted_x_values), type = "response")

### ##########
### Figure 1, Panel B: Petition agreement raw data

png("latex/figures/raw_data_approval.png",6,6, units = "in", res = 300)
cex.size <- 1.5
par(mar = c(5,5,2,2)+0.1)
plot(fitted_x_values, fitted_y_values, type = "l", ylim = c(0,1), xlab = "Relative signature rate", ylab = "Probability of agreement", pch = 19, bty ="n", cex.axis = cex.size, cex.lab = cex.size,xaxt = "n", lwd = 5)
points(x,y, pch = 19, col = alpha("gray",.2))
axis(1, at = seq(0,20,5), cex.axis = cex.size, cex.lab = cex.size)
dev.off()


### ##########
### Tables 2 and S10: Second-level effects, petition approval

vars_for_table <- c("mu_gamma","mu_gamma_new_mp","mu_gamma_cabinet","mu_gamma_margin","mu_alpha_new_mp","mu_alpha_cabinet","mu_alpha_margin","mu_alpha","mu_delta_petition", "mu_delta")

extract_coef_intervals <- function(posterior, vars, model = "", double_gamma_dim = 1){
  
  vars <- vars[vars%in%names(extract(posterior))]

  if(!grepl("double_gamma",model)){
    out <- lapply(vars, function(x){
      extract(posterior, x)
    })  
  } else{
    out <- lapply(vars, function(x){
      tmp <- extract(posterior, x)
      
      if(length(dim(tmp[[1]]))>1) tmp[[1]] <- tmp[[1]][,double_gamma_dim]
      return(tmp)
    })  
  }
  

  est <- unlist(lapply(out, function(x) quantile(x[[1]],0.5)))
  hi <- unlist(lapply(out, function(x) quantile(x[[1]],0.975)))
  lo <- unlist(lapply(out, function(x) quantile(x[[1]],0.025)))
  
  out <- data.frame(vars,est,hi,lo)
  out$model <- model
  return(out)
  
}

full_coefs <- extract_coef_intervals(variational_posterior_full, vars_for_table, "full")
simple_coefs <- extract_coef_intervals(variational_posterior_simple, vars_for_table, "simple")
alpha_coefs <- extract_coef_intervals(variational_posterior_alpha, vars_for_table, "alpha")
delta_coefs <- extract_coef_intervals(variational_posterior_delta, vars_for_table, "delta")

create_cis <- function(coefs){
  coefs$print <- paste0("\\makecell{",round(coefs$est,2),"\\\\","(",round(coefs$lo,2),", ", round(coefs$hi,2),")}")
  return(coefs)
}

simple_coefs <- create_cis(simple_coefs)
alpha_coefs <- create_cis(alpha_coefs)
delta_coefs <- create_cis(delta_coefs)
full_coefs <- create_cis(full_coefs)

coef_out <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "vars", all = TRUE),
       list(simple_coefs[,c("vars","print")],
            alpha_coefs[,c("vars","print")],
            delta_coefs[,c("vars","print")],
            full_coefs[,c("vars","print")]))

names(coef_out) <- c("Variable", "Model 1", "Model 2", "Model 3", "Model 4")

coef_out <- coef_out[match(c("mu_gamma_margin", "mu_gamma_cabinet", "mu_gamma_new_mp","mu_gamma","mu_alpha_margin","mu_alpha_cabinet", "mu_alpha_new_mp","mu_alpha"),coef_out$Variable),]

coef_out$Variable <- paste0("\\textbf{",c("$\\varphi_{\\text{Margin}}$", "$\\varphi_{\\text{Frontbench}}$","$\\varphi_{\\text{New MP}}$","$\\varphi_{0}$","$\\mu_{\\text{Margin}}$","$\\mu_{\\text{Frontbench}}$","$\\mu_{\\text{New MP}}$","$\\mu_0$"),"}")

party_ticks <- c("$\\varphi$ Party dummies", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
party_ticks_two <- c("$\\mu$ Party dummies", "$\\times$", "$\\checkmark$", "$\\times$", "$\\checkmark$")
mp_ticks <- c("MP random effects", "$\\times$", "$\\checkmark$", "$\\times$", "$\\checkmark$")
debate_ticks <- c("Debate random effects", "$\\times$", "$\\times$", "$\\checkmark$", "$\\checkmark$")

N_obs_row <- c("N observations",rep(forStan$N_obs, dim(coef_out)[2]-1))
N_mps <- c("N MPs",rep(forStan$N, dim(coef_out)[2]-1))
N_debates <- c("N Debates",rep(forStan$D, dim(coef_out)[2]-1))

DIC <- c("DIC",round(c(mean(dic_simple), mean(dic_alpha), mean(dic_delta), mean(dic_full)),1))

N_parameters <- c("N parameters",c(length(names(variational_posterior_simple)), length(names(variational_posterior_alpha)), length(names(variational_posterior_delta)), length(names(variational_posterior_full)))-1)

coef_out <- rbind(coef_out, party_ticks, party_ticks_two, mp_ticks, debate_ticks, N_parameters, DIC) #N_obs_row, N_mps, N_debates

alignment <- c("|c","|c","|c","|c","|c","|c|")

to_print <- capture.output(print(xtable(coef_out, align = alignment, label = "tab:part_results"), sanitize.text.function = identity, include.rownames = F, caption.placement = "top",  scalebox = .95, hline.after = c(0,4,8, dim(coef_out)[1]), floating = F))


to_print[grep("Model",to_print)] <- "\\hline  & \\textbf{Model 1} & \\textbf{Model 2} & \\textbf{Model 3} & \\textbf{Model 4} \\\\ "

sink(paste0("latex/tables/multilevel_model_estimates_approval.tex"))
cat(paste(to_print, collapse = "\n"),"\n")
sink()


sink(paste0("latex/tables/multilevel_model_estimates_approval_gamma_only.tex"))
cat(paste(to_print[c(1:11,17,19:25)], collapse = "\n"),"\n")
sink()

#### ##########################
#### Second level effects single gamma model
#### ##########################

variational_posterior <- variational_posterior_full

alpha_0_chain <- extract(variational_posterior, "alpha_0")[[1]]
alpha_chain <- extract(variational_posterior, "alpha")[[1]]
delta_chain <- extract(variational_posterior, "delta")[[1]]
gamma_chain <- extract(variational_posterior, "gamma")[[1]]

alpha_0_chain <- extract(variational_posterior, "mu_alpha")[[1]]
alpha_new_mp_chain <- extract(variational_posterior, "mu_alpha_new_mp")[[1]]
alpha_cabinet_chain <- extract(variational_posterior, "mu_alpha_cabinet")[[1]]
alpha_margin_chain <- extract(variational_posterior, "mu_alpha_margin")[[1]]

gamma_0_chain <- extract(variational_posterior, "mu_gamma")[[1]]
gamma_new_mp_chain <- extract(variational_posterior, "mu_gamma_new_mp")[[1]]
gamma_cabinet_chain <- extract(variational_posterior, "mu_gamma_cabinet")[[1]]
gamma_margin_chain <- extract(variational_posterior, "mu_gamma_margin")[[1]]

delta_0_chain <- extract(variational_posterior, "mu_delta")[[1]]

sigma_alpha_chain <- extract(variational_posterior, "sigma_alpha")[[1]]
sigma_delta_chain <- extract(variational_posterior, "sigma_delta")[[1]]
sigma_gamma_chain <- extract(variational_posterior, "sigma_gamma")[[1]]


## Change in probability

variational_posterior <- variational_posterior_full

alpha_0_chain <- extract(variational_posterior, "alpha_0")[[1]]

mu_alpha_chain <- extract(variational_posterior,"mu_alpha")[[1]]

mu_delta_chain <- extract(variational_posterior,"mu_delta")[[1]]

gamma_0_chain <- extract(variational_posterior, "mu_gamma")[[1]]
gamma_new_mp_chain <- extract(variational_posterior, "mu_gamma_new_mp")[[1]]
gamma_margin_chain <- extract(variational_posterior, "mu_gamma_margin")[[1]]

quantile(forStan$X_sig)
signature_values <- c(-.5,5)

linpred <- sapply(signature_values, function(x) alpha_0_chain + mu_alpha_chain + mu_delta_chain + alpha_new_mp_chain  + (gamma_0_chain + (gamma_margin_chain * mean(forStan$Xi_margin))) * x)

linpred_mean <- apply(linpred,2,mean)
linpred_lo <- apply(linpred,2,quantile,0.025)
linpred_hi <- apply(linpred,2,quantile,0.975)
inv_logit <- function(x) exp(x)/(1+exp(x))

write(round(diff(inv_logit(linpred_mean))*100), file = "latex/useful_numbers/low_to_high_sig_difference_approval.tex")

