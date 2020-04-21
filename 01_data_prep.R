##### ####################################################
#####                                               ######
#####         Prepare speeches data for analysis            
#####                                               ######
##### ####################################################

# init ------------------------------------------------------------

rm(list= ls())

## Load libraries

library(RMariaDB) # 1.0.6
library(data.table) # 1.11.4
library(zoo) # 1.8.4
library(quanteda) # 1.3.4
library(plyr) # 1.8.4
library(XML) # 3.98.1.15

source("utils.R")

## ##############
## TWFY data

load("data/debates_for_replication.Rdata")

## ##############
## Petition data

load("data/petitions_for_replication.Rdata")

write(nrow(petitions_meta), file = "out/tables/useful_numbers/n_petitions.tex")

write(nrow(petitions_meta[petitions_meta$debate_location != "Commons Chamber",]), file = "out/tables/useful_numbers/n_petitions_wmin_hall.tex")

## ##############
# Combine petitions with the same url

tmp_sig_list <- list()
i <- 0
ons_codes <- petition_signatures_by_constituency_list[[1]]$ons_code

for(url in petitions_meta$debate_transcript_url){
  i <- i+1
  to_combine <- which(petitions_meta$debate_transcript_url == url)
  
  if(length(to_combine)==1) tmp_sig_list[[i]] <- petition_signatures_by_constituency_list[[i]]
  if(length(to_combine)!=1) {
    
    this_petition_id <- unique(rbind.fill(petition_signatures_by_constituency_list[to_combine])$petition_id)[1]
    
    tmp_sig_list[[i]] <- data.frame(data.table(rbind.fill(petition_signatures_by_constituency_list[to_combine]))[,list(petition_id = this_petition_id, name = unique(name),mp = unique(mp), signature_count = sum(signature_count,na.rm = T)),by = ons_code])
    
  }
  
}

duplicated_petitions <- which(duplicated(petitions_meta$debate_transcript_url))

petitions_meta <- petitions_meta[-duplicated_petitions,]
petition_signatures_by_constituency_list <- tmp_sig_list[-duplicated_petitions]

## ##############
## Merge petitions with debate data

petition_to_twfy_mapping <- read.csv(file = "data/petition_to_twfy_mapping.csv") # Manually coded file
petition_to_twfy_mapping$hdate <- as.Date(petition_to_twfy_mapping$debate_debated_on,"%d/%m/%Y")
petition_to_twfy_mapping$petition_id <- petition_to_twfy_mapping$id

debates <- merge(debates, petition_to_twfy_mapping[,c("petition_id","hdate","subsection_title_twfy")], by.x = c("hdate","subsection_title"), by.y = c("hdate", "subsection_title_twfy"), all.x = T)

pet_sigs <- data.table(rbind.fill(petition_signatures_by_constituency_list))

debate_locations_by_id <- data.table(petitions_meta)[,unique(debate_location),by = id]

pet_sigs$location <- debate_locations_by_id$V1[match(pet_sigs$petition_id, debate_locations_by_id$id)]

## #############
## Reshape to MP-by-debate

debates[,mId := as.factor(as.character(person_id)),] # Make mp_id a factor (required for the next step)

debates$dId <- debates$subsection_id_new


mp_by_debate <- debates[,list(mId = levels(mId), N = c(table(mId))), dId] # Find number of times each MP appears in each debate
mp_by_debate$N <- as.numeric(mp_by_debate$N > 0)

## Add debate information
debates$subsection_title <- as.character(debates$subsection_title)

mp_by_debate <- merge(mp_by_debate, debates[,list(debate_date = unique(hdate), debate_title = unique(subsection_title)[1], petition_id = unique(petition_id)[1]),by = dId])

## Add MP information

debates$mId <- as.character(debates$mId)

mp_by_debate <- merge(mp_by_debate, debates[,list(party = unique(party_short)[1], name = unique(name)[1], mnis_person_id = unique(mnis_person_id), ONSCode = unique(ONSCode)), by = mId], by = "mId")

## Drop all MPs who weren't serving during a given period of debate

load("data/mps_start_end_for_replication.Rdata")

mps_start_end <- data.table(mps_start_end)
setkey(mps_start_end, Member_Id, HouseStartDate, HouseEndDate)
setkey(mp_by_debate, mnis_person_id, debate_date)

mps_start_end$Member_Id <- as.numeric(as.character(mps_start_end$Member_Id))

mp_by_debate <- merge(mp_by_debate, mps_start_end[,c("Member_Id","HouseStartDate","HouseEndDate")], by.x = "mnis_person_id", by.y = "Member_Id")

mp_by_debate <- mp_by_debate[mp_by_debate$debate_date >= mp_by_debate$HouseStartDate & mp_by_debate$debate_date <= mp_by_debate$HouseEndDate]

## Add Signature information

mp_by_debate <- merge(mp_by_debate, pet_sigs[,c("petition_id", "ons_code","signature_count","location")], by.x = c("petition_id", "ONSCode"), by.y = c("petition_id", "ons_code"), all.x = T)

## Clean up signature count variable

mp_by_debate[,petition_debate := any(!is.na(signature_count)),by = dId]

mp_by_debate$signature_count[is.na(mp_by_debate$signature_count) & mp_by_debate$petition_debate == T] <- 0

# Merge in constituency level data

votereg <- read.csv("data/constituency_votereg.csv")[,-2]
votereg <- votereg[!is.na(votereg$Total2015),]
votereg$ONSCode <- as.character(votereg$ONSCode)

mp_by_debate <- merge(mp_by_debate, votereg, by = "ONSCode", all.x = T)

## #############
## DVs

mp_by_debate[, sig_rate := (signature_count)/Total2015]

mp_by_debate[, national_sig_rate := sum(signature_count, na.rm = T)/sum(Total2015), by = dId]
mp_by_debate$national_sig_rate[mp_by_debate$national_sig_rate==0] <- NA

mp_by_debate$relative_sig_rate <- mp_by_debate$sig_rate/mp_by_debate$national_sig_rate
mp_by_debate$relative_sig_rate <- mp_by_debate$relative_sig_rate-1
mp_by_debate$relative_sig_rate[is.na(mp_by_debate$relative_sig_rate)] <- 0

av_sig_rate <- mp_by_debate[, list(constituency_average_sig_rate = mean(sig_rate, na.rm = T)), by = mId]
mp_by_debate <- merge(mp_by_debate, av_sig_rate, by = "mId")
mp_by_debate$relative_constituency_sig_rate <- mp_by_debate$sig_rate/mp_by_debate$constituency_average_sig_rate
mp_by_debate$relative_constituency_sig_rate <- mp_by_debate$relative_constituency_sig_rate - 1
mp_by_debate$relative_constituency_sig_rate[is.na(mp_by_debate$relative_constituency_sig_rate)] <- 0

mp_by_debate$new_data <- mp_by_debate$debate_date >= as.Date("2017-06-7")

###################################
## Add in constituency marginality 

library(readstata13) # CRAN v0.9.2

bes <- read.dta13("data/BES-2017-General-Election-results-file-v1.0.dta")

mp_by_debate$margin <- NA

mp_by_debate$margin[!mp_by_debate$new_data] <- bes$Majority15[match(mp_by_debate$ONSCode[!mp_by_debate$new_data],bes$ONSConstID)]
mp_by_debate$margin[mp_by_debate$new_data] <- bes$Majority17[match(mp_by_debate$ONSCode[mp_by_debate$new_data],bes$ONSConstID)]
mp_by_debate <- mp_by_debate[!is.na(margin)]

###################################
## Add in new MP dummy

mp_by_debate$new_mp <- as.numeric(mp_by_debate$HouseStartDate >= as.Date("2015-05-05"))

###################################
## Add in cabinet minister dummy
###################################

load("data/frontbenchers.Rdata")

ministers$Member_Id <- as.numeric(as.character(ministers$Member_Id))
shadow_ministers$Member_Id <- as.numeric(as.character(shadow_ministers$Member_Id))

setkey(ministers, Member_Id, StartDate, EndDate)
setkey(shadow_ministers, Member_Id, StartDate, EndDate)
mp_by_debate$debate_date2 <- mp_by_debate$debate_date
setkey(mp_by_debate, debate_date, debate_date2)

# Subset only to those who hold frontbench positions
ministers <- ministers[ministers$PostCabinet%in%c(2,3,4)]
shadow_ministers <- shadow_ministers[shadow_ministers$PostCabinet%in%c(2,3,4)]

minister_matches <- foverlaps(mp_by_debate, ministers[,c("Member_Id", "StartDate", "EndDate")], 
                              by.x = c("mnis_person_id", "debate_date", "debate_date2"),
                              by.y = c("Member_Id", "StartDate", "EndDate"), mult = "first", which = T)

shadow_minister_matches <- foverlaps(mp_by_debate, shadow_ministers[,c("Member_Id", "StartDate", "EndDate")], 
                                     by.x = c("mnis_person_id", "debate_date", "debate_date2"),
                                     by.y = c("Member_Id", "StartDate", "EndDate"), mult = "first", which = T)

mp_by_debate$shadow_minister <- mp_by_debate$minister <- F
mp_by_debate$shadow_minister[!is.na(shadow_minister_matches)] <- T
mp_by_debate$minister[!is.na(minister_matches)] <- T
mp_by_debate$frontbench <- mp_by_debate$minister | mp_by_debate$shadow_minister

# Make sure that Boris is a minister, but not a new MP

mp_by_debate[mnis_person_id == 1423]$new_mp <- 0

## Add dummy variable for whether the MP was a member of the petitions committee

library(xml2) # CRAN v1.2.2

petition_committee <- read_xml("data/petition_committee_members.xml")
petition_committee_members <- xml_find_all(petition_committee,"//Members/Member")

petition_committee_out <- data.frame(matrix(NA, length(petition_committee_members), 4))
names(petition_committee_out) <- c("mnis_id", "name","start","end")
for(i in 1:length(petition_committee_members)){
  tmp <- petition_committee_members[[i]]  
  tmp_id <- xml_attr(tmp,"Member_Id")
  tmp_names <- xml_text(xml_find_all(tmp,"./DisplayAs"))
  tmp_started <- min(as.Date(xml_text(xml_find_all(tmp,"./CommitteeMemberships/CommitteeMembership/StartDate"))))
  tmp_ended <- xml_text(xml_find_all(tmp,"./CommitteeMemberships/CommitteeMembership/EndDate"))
  tmp_ended[tmp_ended == ""] <- as.character(Sys.Date())
  tmp_ended <- max(as.Date(tmp_ended))  
  petition_committee_out[i,1] <- tmp_id
  petition_committee_out[i,2] <- tmp_names
  petition_committee_out[i,3] <- tmp_started
  petition_committee_out[i,4] <- tmp_ended
  
}
petition_committee_out$start <- as.Date(petition_committee_out$start)
petition_committee_out$end <- as.Date(petition_committee_out$end)

petition_committee_out <- data.table(petition_committee_out)
petition_committee_out$mnis_id <- as.numeric(petition_committee_out$mnis_id)
setkey(petition_committee_out, mnis_id, start, end)
setkey(mp_by_debate, debate_date, debate_date2)

petition_committee_matches <- foverlaps(mp_by_debate, petition_committee_out[,c("mnis_id", "start", "end")], 
                              by.x = c("mnis_person_id", "debate_date", "debate_date2"),
                              by.y = c("mnis_id", "start", "end"), mult = "first", which = T)

mp_by_debate$petitions_committee <- F
mp_by_debate$petitions_committee[!is.na(petition_committee_matches)] <- T

# Subset to only relatively popular debates

mp_by_debate[,n_in_debate := length(mId[N!=0]) ,by = dId]
mp_by_debate <- mp_by_debate[mp_by_debate$n_in_debate > 5] 

## Keep only those debates left in the sample

debates <- debates[debates$dId%in%unique(mp_by_debate$dId)]

## Save

save(mp_by_debate, debates, file = "working/mp_by_debate.Rdata")


