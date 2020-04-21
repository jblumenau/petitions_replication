##### ####################################################
#####                                               ######
#####           Topic models of debates             
#####                                               ######
##### ####################################################

# init ------------------------------------------------------------

rm(list=ls())

# Load libraries

library(data.table) # CRAN v1.12.2
library(quanteda) # CRAN v1.5.1
library(stm) # CRAN v1.3.3
library(stargazer) # CRAN v5.2.2
library(xtable) # CRAN v1.8-4
library(lsa) # CRAN v0.73.1
library(toOrdinal) # CRAN v1.1-0.0
source("utils.R") # Included in replication package

# Load data

load("working/mp_by_debate.Rdata")

K <- seq(5, 100, 5)

# Prepare debates for topic modelling ----------------------------------------------- 

# Reduce number of debates

debates$body <- as.character(debates$body)

# Remove duplicated speeches by the same member in the same debate
debates <- debates[-which(duplicated(paste0(debates$mId,debates$subsection_id,debates$body)))]

debate_concat <- debates[,list(
  debate_text = paste(body, collapse = " "),
  debate_date = unique(hdate)
),by = dId]

# Run topic model ------------------------------------------------------------ 

debateCorpus <- corpus(debate_concat$debate_text, docvars = data.frame(debate_concat)[,-grep("^debate_text$", names(debate_concat))])

debateDFM <- dfm(debateCorpus, remove = stopwords("en", source = "smart"), stem = T, remove_punct = T, remove_numbers = T)

debateDFM <- dfm_trim(debateDFM, min_docfreq = .025, docfreq_type = "prop")

save(debate_concat, debateCorpus, debateDFM, file = "working/text_objects.Rdata")

## Loop over topic counts

for(k in K){
  
  time_elapsed <- system.time({
    ldaOutSTM <- stm(debateDFM, K = k, init.type = "Spectral", verbose = T, emtol = 1e-3, seed = 221186)
  })
  print(paste0(k, " topics took ", time_elapsed[3], " seconds."))
  
  debate_proportions <- data.frame(ldaOutSTM$theta)
  topic_labels <- apply(labelTopics(ldaOutSTM)$frex,1,function(x) paste0(x, collapse = "_"))
  names(debate_proportions) <- topic_labels
  
  save(debate_proportions, topic_labels, file = paste0("working/topicmodel/ldaOut_",k,".Rdata"))
  print(topic_labels)

  }


### ################################################
### TABLE AG: Closest matches for each petition debate in the non-petition debates
### ################################################

for(k in K){
  print(k)
  load(paste0("working/topicmodel/ldaOut_",k,".Rdata"))
  
  debate_titles <- mp_by_debate[,list(debate_title= unique(debate_title)),by = dId]
  
  word_split <- function(examp, n_words){
    x <- unlist(strsplit(examp, "\\s+"))
    y <- split(x, seq_along(x)%/%n_words)
    y <- paste0(y[[1]], collapse = " ")
    return(y)
  }
  
  petition_debates <- unique(mp_by_debate$dId[mp_by_debate$petition_debate])
  #this_did <- petition_debates[69]
  #n_matches = 3
  find_closest_debates <- function(this_did, n_matches = 6){
    
    petition_debate_title <- unique(mp_by_debate[mp_by_debate$dId == this_did]$debate_title)
    
    debate_index <- which(debate_concat$dId == this_did)
    
    debate_topic_props <- as.numeric(debate_proportions[debate_index,])
    tmp <- apply(debate_proportions,1, function(x) cosine(x,debate_topic_props))
    top_100 <- debate_concat$dId[order(tmp, decreasing = T)[1:100]]
    top_100 <- top_100[top_100 != this_did]
    top_100 <- debate_titles$debate_title[match(top_100, debate_titles$dId)]
    
    top_100 <- trim(gsub("\\s*\\[[^\\)]+\\]|\\s*\\([^\\)]+\\)|&#8212;|&#8221|&#8220|3rd Alloted day|Backbench Business|Opposition Day","", top_100))
    top_100 <- trim(gsub("&amp;","&",top_100))
    top_100 <- trim(gsub("&#8217;","'",top_100))
    
    petition_debate_title <- trim(gsub("\\s*\\[[^\\)]+\\]|\\s*\\([^\\)]+\\)|&#8212;|&#8221|&#8220|3rd Alloted day|Backbench Business|Opposition Day","", petition_debate_title))
    petition_debate_title <- trim(gsub("&amp;","&",petition_debate_title))
    petition_debate_title <- trim(gsub("&#8217;","'",petition_debate_title))
    
    top_100 <- top_100[!top_100%in%c("Topical Questions","Environment, Food and Rural Affairs Committee, Environmental Audit Committee, Health and Social Care Committee and Transport Committee")]
    top_100 <- top_100[top_100!=""]
    closest_matches <- paste0(top_100[1:n_matches], collapse = "; ")
    
    return(c(petition_debate_title, closest_matches))
    
  }
  
  closest_debates <- data.frame(matrix(NA, length(petition_debates), 2))
  for(i in 1:length(petition_debates)) closest_debates[i,] <- find_closest_debates(petition_debates[i], n_matches = 3)
  closest_debates$X2 <- gsub("&"," and ",closest_debates$X2)
  names(closest_debates)[1] <- "Petition Debate"
  names(closest_debates)[2] <- "Closest Matching Debates"
  
  closest_debates$date <- debate_concat$debate_date[debate_concat$dId %in% petition_debates]
  closest_debates <- closest_debates[order(closest_debates$date),c("Petition Debate", "Closest Matching Debates")]
  
  closest_debates$`Petition Debate` <- paste0("\\textbf{",closest_debates$`Petition Debate`,"}")
  names(closest_debates) <- paste0("\\textbf{",names(closest_debates),"}")
  
  sink(paste0("out/tables/closest_debates/",k,"_topics.tex"))
  print(xtable(closest_debates, align = c("p{0\\textwidth}|","|p{0.4\\textwidth}|","p{0.6\\textwidth}|"), caption = "Closest matching debates", label = "tab:closest_matching_debates"), include.rownames = F, size = "scriptsize",sanitize.text.function = function(x) {
    x <- gsub('%', '\\\\%', x)
    x <- gsub('&', 'and', x)
  }
  , tabular.environment="longtable", floating=FALSE, hline.after = seq(-1,nrow(closest_debates),1))
sink()  

}

