##### ####################################################
#####                                               ######
#####             Descriptive analyses
#####                                               ######
##### ####################################################

# init ------------------------------------------------------------

rm(list=ls())

# Load libraries

library(data.table) # 1.11.4
library(scales) # 1.0.0
library(lsa) # 0.73.1
library(mgcv) # 1.8.24
library(stargazer) # 5.2.2

# Load data

load("working/mp_by_debate.Rdata")
load("working/best_k.Rdata")
load(paste0("working/topicmodel/ldaOut_",best_k,".Rdata"))
load("working/text_objects.Rdata")

## ##############
## Table S7, Confounding by closest matching prior non-petition debates

mp_by_debate <- data.table(mp_by_debate, debate_proportions[match(mp_by_debate$dId, debate_concat$dId),])

unique_topics <- mp_by_debate[,lapply(.SD, unique), by = dId, .SDcols = topic_labels]

petition_debates <- mp_by_debate[,list(petition_debate = unique(petition_debate), debate_date = unique(debate_date)),by = dId]

unique_topics <- merge(unique_topics, petition_debates, by = "dId")

petition_did <- 5636

pairs_func <- function(petition_did){
  
  petition_date <- unique_topics[dId == petition_did]$debate_date
  
  petition_topic_dist <- unique_topics[dId == petition_did]
  petition_topic_dist <- petition_topic_dist[,-1]
  petition_topic_dist <- petition_topic_dist[,-dim(petition_topic_dist)[2], with = F]
  petition_topic_dist <- petition_topic_dist[,-dim(petition_topic_dist)[2], with = F]
  petition_topic_dist <- as.numeric(petition_topic_dist)
  
  prior_debate_dists <- unique_topics[debate_date < petition_date]
  prior_debate_dists <- prior_debate_dists[petition_debate==F]
  prior_debate_dids <- prior_debate_dists$dId
  
  prior_debate_dists <- prior_debate_dists[,-1]
  prior_debate_dists <- prior_debate_dists[,-dim(prior_debate_dists)[2], with = F]
  prior_debate_dists <- prior_debate_dists[,-dim(prior_debate_dists)[2], with = F]
  
  prior_debate_dists <- as.matrix(prior_debate_dists)
  
  prior_debate_sims <- apply(prior_debate_dists,1, function(x) cosine(x,petition_topic_dist))
  
  closest_match <- prior_debate_dids[order(prior_debate_sims, decreasing = T)[1:3]]
  
  return(c(petition_did,  closest_match)
  )
  
}

closest_matches <- do.call("rbind",lapply(1:length(unique_topics[petition_debate==T]$dId), function(x) pairs_func(unique_topics[petition_debate==T]$dId[x])))

find_closest_sigs <- function(matches, new_title = F){
  
  petition_did <- as.numeric(matches[1])
  
  closest_matches_dids <- as.numeric(matches[-1])
  
  petition_debate <- mp_by_debate[dId == petition_did]  
  
  # Did the MP participate in the closest matching debate?
  
  participated_mat <- matrix(NA, length(petition_debate$mId), length(closest_matches_dids))
  
  for(i in 1:length(closest_matches_dids)){
    closest_match_debate <- mp_by_debate[dId == closest_matches_dids[i]]    
    participated <- closest_match_debate$N[match(petition_debate$mId,closest_match_debate$mId)]==1
    participated[is.na(participated)] <- F
    participated_mat[,i] <- participated
  }
  
  petition_debate$N_closest_any <- rowSums(participated_mat) > 0
  
  if(new_title == T) petition_debate$debate_title_new <- unique(mp_by_debate[dId == closest_matches_dids[1]]$debate_title)
  
  return(petition_debate)
  
}

closest_sigs_dfs <- do.call("rbind",lapply(1:dim(closest_matches)[1], function(x) find_closest_sigs(closest_matches[x,])))

closest_sigs_dfs <- closest_sigs_dfs[closest_sigs_dfs$relative_sig_rate < 60]

true_mod <- lm(N ~ relative_sig_rate, data= closest_sigs_dfs)
closest_mod <- lm(N_closest_any ~ relative_sig_rate, data= closest_sigs_dfs)

sink(paste0("latex/useful_numbers/true_linear_5_point_effect.txt"))
cat(round((coef(true_mod)[2]*5)*100))
sink()

sink(paste0("latex/useful_numbers/closest_linear_5_point_effect.txt"))
cat(round((coef(closest_mod)[2]*5)*100))
sink()

sink(paste0("latex/tables/closest_matching_confounding_regression.tex"))
stargazer(true_mod, closest_mod, keep.stat = c("rsq","n"), dep.var.caption = "Participation", dep.var.labels = c("Petition Debates", "Closest matching non-petition debates"), covariate.labels = c("Relative Signature Rate"), label = "tab:closest_matching_regression_ests")
sink()

## ##############
## Figure 2, Participation in non-petition debates

closest_sigs_dfs_ind <- do.call("rbind",lapply(1:dim(closest_matches)[1], function(x) find_closest_sigs(closest_matches[x,],new_title = T)))
closest_sigs_dfs_ind <- closest_sigs_dfs_ind[closest_sigs_dfs_ind$relative_sig_rate < 60]

gam_plot <- function(petition_did, K =10,...){
  
  x <- closest_sigs_dfs_ind[dId == petition_did]$relative_sig_rate
  y <- closest_sigs_dfs_ind[dId == petition_did]$N
  y2 <- closest_sigs_dfs_ind[dId == petition_did]$N_closest_any
  
  petition_title <- as.character(unique(closest_sigs_dfs_ind[dId == petition_did]$debate_title))
  new_title <- as.character(unique(closest_sigs_dfs_ind[dId == petition_did]$debate_title_new))
  
  gam_model <- gam(y ~ s(x, k = K))
  gam_model2 <- gam(y2 ~ s(x, k = K))
  
  fitted_x_values <- seq(-1,max(x),0.01)
  fitted_y_values <- predict(gam_model, newdata = data.frame(x = fitted_x_values), type = "response")
  fitted_y_values2 <- predict(gam_model2, newdata = data.frame(x = fitted_x_values), type = "response")
  
  plot(fitted_x_values, fitted_y_values2, ylim = c(0,1),  type = "l", lwd = 5, bty = "n",...)
  points(x,y2, pch = 19, col = alpha("gray",.2),...)
}



png("latex/figures/refugee_confounding.png", 6 , 6, units = "in", res = 300)
cex.size <- 1.5
par(mar = c(5,5,2,2)+0.1, mfrow = c(1,1))
gam_plot(unique(closest_sigs_dfs_ind$dId)[1], K = 5, 
         xlab = "'Refugee' signature rate",
         ylab = "Non-petition debate participation", cex = 1.5, 
         cex.axis = cex.size, cex.lab = cex.size, xlim = c(-1,4.5))
dev.off()

png("latex/figures/eu_confounding.png", 6, 6, units = "in", res = 300)
cex.size <- 1.5
par(mar = c(5,5,2,2)+0.1, mfrow = c(1,1))
gam_plot(unique(closest_sigs_dfs_ind$dId)[75], K = 5, 
         xlab = "'EU' signature rate",
         ylab = "Non-petition debate participation", cex = 1.5, 
         cex.axis = cex.size, cex.lab = cex.size, xlim = c(-1,4.5))
dev.off()

