##### ####################################################
#####                                               ######
#####                 Petition analysis             
#####                                               ######
##### ####################################################

# init ------------------------------------------------------------

rm(list=ls())

library(data.table) # CRAN v1.12.2
library(plyr) # CRAN v1.8.4
library(scales) # CRAN v1.0.0
library(readstata13) # CRAN v0.9.2
library(stargazer) # CRAN v5.2.2
library(zoo) # CRAN v1.8-6
library(xtable) # CRAN v1.8-4

## ##############
## Load data

load("data/petitions_for_replication.Rdata")

## ##############
## Table S3 Constituency Signature Predictors

seatdata <- suppressWarnings(read.dta13("data/BES-2015-General-Election-results-file-v2.2.dta"))

eurefdata <- read.csv("data/Hanretty-Constituency-Estimates.csv")
names(eurefdata)[names(eurefdata)=="Figure.to.use"] <- "LeaveShare"

seatdata <- merge(seatdata, eurefdata[,c("PCON11CD","LeaveShare")], by.x = "ONSConstID", by.y = "PCON11CD")

petitions_to_keep <- which(petitions_meta$signature_count>1000)

petition_signatures_by_constituency_list <- petition_signatures_by_constituency_list[petitions_to_keep]
petitions_meta <- petitions_meta[petitions_to_keep,]

constituencies <- petition_signatures_by_constituency_list[[1]]$ons_code

for(p in 1:length(petition_signatures_by_constituency_list)){
  
  if(dim(petition_signatures_by_constituency_list[[p]])[1] == length(constituencies)) {
    next
  }else{
    cat(".")
    test <- petition_signatures_by_constituency_list[[p]]
    test <- merge(data.frame(ons_code = constituencies), test, by = "ons_code", all = T)
    test$petition_id <- as.numeric(na.omit(unique(test$petition_id)))
    test$signature_count[is.na(test$signature_count)] <- 0
    petition_signatures_by_constituency_list[[p]] <- test
  }
  
}

all_sigs <- rbind.fill(petition_signatures_by_constituency_list[unlist(lapply(petition_signatures_by_constituency_list,class))=="data.frame"])

all_sigs <- merge(all_sigs, seatdata, by.x = "ons_code", by.y = "ONSConstID")

all_sigs$Winner15Simple <- as.character(all_sigs$Winner15)
all_sigs$Winner15Simple[all_sigs$Winner15Simple%in%c("Plaid Cymru", "ukip","Green")] <- "Other"
all_sigs$Winner15Simple[all_sigs$Winner15Simple == "Scottish National Party"] <- "SNP"
all_sigs$Winner15Simple[all_sigs$Winner15Simple == "Liberal Democrat"] <- "LibDem"
all_sigs$Winner15Simple <- factor(all_sigs$Winner15Simple, levels = c("Conservative", "Labour", "LibDem", "SNP", "Other"))

all_sigs$young <- all_sigs$c11Age16to17 + all_sigs$c11Age18to19 + all_sigs$c11Age20to24 + all_sigs$c11Age25to29

all_sigs$Region <- relevel(all_sigs$Region, "South East")

## Income

income <- read.csv("data/annual_income.csv")
income$mean_pay <- as.character(income$mean_pay)
income$median_pay <- as.character(income$median_pay)
income$mean_pay[income$mean_pay == "x"] <- income$median_pay[income$mean_pay == "x"]
income$mean_pay[income$mean_pay == "x"] <- NA
income$mean_pay <- as.numeric(gsub(",","", income$mean_pay))
income$mean_pay <- income$mean_pay/1000

income$median_pay <- as.numeric(gsub(",","", income$median_pay))
income$median_pay <- income$median_pay/1000

population <- read.csv("data/population_2016.csv")

population$ons_code <- as.character(population$ons_code)

income$ons_code <- as.character(income$ons_code)
income <- income[!income$ons_code == "",]

income <- merge(income, population[,c("ons_code", "Total15")], by = "ons_code")

all_sigs <- data.table(all_sigs)
sig_counts <- all_sigs[,list(total_signatures = sum(signature_count)),by = ons_code]

income <- merge(income,sig_counts, by = "ons_code")

income <- merge(income, seatdata, by.x = "ons_code", by.y = "ONSConstID")

income$sig_weight <- (income$total_signatures/income$Total15)

income$young <- income$c11Age16to17 + income$c11Age18to19 + income$c11Age20to24 +income$c11Age25to29

levels(income$Winner15)[levels(income$Winner15)%in%c("Green","ukip", "Plaid Cymru")] <- "Other"
levels(income$Winner15)[levels(income$Winner15)%in%c("Scottish National Party")] <- "SNP"
levels(income$Winner15)[levels(income$Winner15)%in%c("Liberal Democrat")] <- "Lib Dem"
levels(income$Region)[levels(income$Region)%in%c("Yorkshire and The Humber")] <- "Yorkshire"

income$median_pay[is.na(income$median_pay)] <- mean(income$median_pay, na.rm = T)

mod.one <- lm(sig_weight ~  median_pay + c11Female + young + c11PopulationDensity + c11FulltimeStudent+ c11IndustryManufacturing + c11HouseOwned + c11EthnicityWhiteBritish + c11Unemployed + LeaveShare + Winner15 ,data = income)
mod.region.fe <- lm(sig_weight ~  median_pay + c11Female + young + c11PopulationDensity + c11FulltimeStudent+ c11IndustryManufacturing + c11HouseOwned + c11EthnicityWhiteBritish + c11Unemployed + LeaveShare + Winner15+Region ,data = income)

covars <- c("Median pay","Female", "Young", "Pop. Density", "Students", "Industry", "Home Ownership", "White", "Unemployed", "Leave Share", "Labour 2015", "LibDem 2015", "SNP 2015", "Other 2015")

sink("latex/tables/constituency_signature_predictors.tex") 
stargazer(mod.one, mod.region.fe, omit = c("Region"), covariate.labels = covars, keep.stat = c("n","rsq"), title = "Constituency-level determinants of signature rates", label = "tab:constituency_predictors", no.space = T, dep.var.caption = "", dep.var.labels = "Signatures per capita", add.lines = list(c("Regional Fixed-Effects", "No", "Yes")), table.placement = "h")
sink()


## ##############
## Table S4 Debated Petitions

petitions_meta <- petitions_meta[!is.na(petitions_meta$debate_transcript_url),]
petitions_meta$action <- paste0("\\href{",petitions_meta$debate_transcript_url,"}{",petitions_meta$action,"}")

petitions_meta <- data.frame(Title = petitions_meta$action, Signatures = petitions_meta$signature_count, Date = as.character(as.yearmon(petitions_meta$debate_debated_on)))

sink("latex/tables/debated_petitions.tex")
print(xtable(petitions_meta, align = "lllr", caption = "Debated e-petitions", label = "tab:debated_petitions"), include.rownames = F, size = "scriptsize",sanitize.text.function = function(x) {
  x <- gsub('%', '\\\\%', x)
  x <- gsub('&', 'and', x)
}
, tabular.environment="longtable", floating=FALSE, caption.placement = "top")
sink()


