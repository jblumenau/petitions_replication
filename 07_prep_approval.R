##### ####################################################
#####                                               ######
#####                 Approval analysis
#####                                               ######
##### ####################################################

# init ------------------------------------------------------------

rm(list=ls())

# Load libraries

library(data.table) # CRAN v1.12.2
library(rstan) # CRAN v2.19.2
library(stargazer) # CRAN v5.2.2

## Load data

load("working/mp_by_debate.Rdata")

mp_by_debate$mId <- as.numeric(mp_by_debate$mId)

## Load manually coded data

first_parl <- data.table(read.csv("data/agreement_codings/jack_selections.csv", stringsAsFactors = F))
first_parl <- first_parl[!is.na(first_parl$petition_id)]
second_parl <- data.table(read.csv("data/agreement_codings/jack_selections_17_19.csv", stringsAsFactors = F))

## Merge

mp_by_debate <- merge(mp_by_debate, second_parl[,c("first", "mId", "petition_id")], by.x = c("mId","petition_id"), by.y = c("mId","petition_id"), all.x = T)

mp_by_debate <- merge(mp_by_debate, first_parl[,c("first", "mp_id", "petition_id")], by.x = c("mnis_person_id","petition_id"), by.y = c("mp_id","petition_id"), all.x = T)

mp_by_debate$first.x[!is.na(mp_by_debate$first.y)] <- mp_by_debate$first.y[!is.na(mp_by_debate$first.y)]

## Subset to petition debates and to people who participated
mp_by_pet_debate <- mp_by_debate[petition_debate == T & N == 1]

## Drop "Speakers" of the House

mp_by_pet_debate <- mp_by_pet_debate[!(mp_by_pet_debate$first.x %in% c("NA", "Speaker") | is.na(mp_by_pet_debate$first.x)),]

mp_by_pet_debate$approve <- mp_by_pet_debate$first.x == "For"

###################################
## Prepare stan data
###################################

mp_by_pet_debate$nId <- as.numeric(as.factor(as.character(paste0(mp_by_pet_debate$mId, mp_by_pet_debate$new_data))))
mp_by_pet_debate$dId_tmp <- as.numeric(as.factor(as.character(mp_by_pet_debate$dId)))
mp_by_pet_debate$mId_tmp <- as.numeric(as.factor(as.character(mp_by_pet_debate$mId)))

setkey(mp_by_pet_debate, nId, dId_tmp)

Xi_margin <- mp_by_pet_debate[,unique(margin),by = nId]
setkey(Xi_margin, nId)
Xi_margin <- Xi_margin$V1/100

Xi_new_mp <- mp_by_pet_debate[,unique(new_mp),by = nId]
setkey(Xi_new_mp, nId)
Xi_new_mp <- Xi_new_mp$V1

Xi_frontbench <- mp_by_pet_debate[,any(minister|shadow_minister),by = nId]
setkey(Xi_frontbench, nId)
Xi_frontbench <- Xi_frontbench$V1

mp_parties_vec <- mp_by_pet_debate[,unique(party),by = nId]
setkey(mp_parties_vec, nId)
mp_parties_vec <- mp_parties_vec$V1

mp_parties_vec <- factor(mp_parties_vec, levels = levels(mp_parties_vec)[c(1,3,4,2)])

Xi_party_matrix <- model.matrix(~mp_parties_vec-1)[,-1] # Coonservative as the baseline category

P <- dim(Xi_party_matrix)[2]

forStan = list(
  N_obs = nrow(mp_by_pet_debate),
  N = length(unique(mp_by_pet_debate$nId)),
  M = length(unique(mp_by_pet_debate$mId)),
  D = length(unique(mp_by_pet_debate$dId_tmp)),
  P = P,
  mId = mp_by_pet_debate$mId_tmp,
  nId = mp_by_pet_debate$nId,
  dId = mp_by_pet_debate$dId_tmp,
  Y = mp_by_pet_debate$approve,
  X_sig = mp_by_pet_debate$relative_sig_rate,
  X_sig_constituency = mp_by_pet_debate$relative_constituency_sig_rate,
  X_margin = mp_by_pet_debate$margin,
  X_newmp = mp_by_pet_debate$new_mp,
  X_sig_margin_interaction = mp_by_pet_debate$relative_sig_rate * mp_by_pet_debate$margin,
  Xi_margin = Xi_margin,
  Xi_new_mp = Xi_new_mp,
  Xi_cabinet = Xi_frontbench,
  Xi_party_matrix = Xi_party_matrix
)

labels_mid <- mp_by_pet_debate[,unique(name),by = mId_tmp]
setkey(labels_mid, mId_tmp)
labels_mid <- labels_mid$V1

labels_person_id <- mp_by_pet_debate[,unique(mId),by = mId_tmp]
setkey(labels_person_id, mId_tmp)
labels_person_id <- labels_person_id$V1

labels_dId <- mp_by_pet_debate[,unique(debate_title),by = dId_tmp]
setkey(labels_dId, dId_tmp)
labels_dId <- labels_dId$V1

stanLabels <- list(
  labels_mId = labels_mid,
  labels_person_id = labels_person_id,
  labels_dId = labels_dId
)

save(forStan, stanLabels, mp_by_pet_debate, file = "working/forStan_approval.Rdata")


## Useful numbers

write(forStan$N_obs, file = "latex/useful_numbers/n_observations_approval.tex")
write(sum(forStan$Y), file = "latex/useful_numbers/n_y_1_approval.tex")
write(forStan$M, file = "latex/useful_numbers/n_unique_mps_approval.tex")

## Intercoder reliability

roberta <- data.table(read.csv("data/agreement_codings/roberta_selections.csv", stringsAsFactors = F))
jack <- data.table(read.csv("data/agreement_codings/jack_selections.csv", stringsAsFactors = F))

write(round(mean((roberta$first == "For") == (jack$first == "For"), na.rm = T)*100), file = "latex/useful_numbers/intercoder_agreement.txt")

## ##############
## Table S1 Summary statistics

tmp <- mp_by_debate[!(mp_by_debate$petition_debate == T & mp_by_debate$location == "Commons Chamber"),list(N,
                                              relative_sig_rate,
                                              margin/100,
                                              frontbench,
                                              new_mp)]

names(tmp) <- c("Participate","Signature Rate", "Margin", "Frontbench", "New MP")

sink("latex/tables/summary_statistics.tex")
stargazer(tmp, summary = T, title = "Summary statistics", label = "tab:summary_vars", summary.stat = c("mean","median","max", "min","sd","n"), digits = 2)
sink()

## ##############
## Table S2 Summary statistics, approval

tmp <- mp_by_pet_debate[petition_debate == T,list(approve,
                                              relative_sig_rate,
                                              margin/100,
                                              frontbench,
                                              new_mp)]

names(tmp) <- c("Approve","Signature Rate", "Margin", "Frontbench", "New MP")


sink("latex/tables/summary_statistics_approval.tex")
stargazer(tmp, summary = T, title = "Summary statistics", label = "tab:summary_vars_approval", summary.stat = c("mean","median","max", "min","sd","n"), digits = 2)
sink()
