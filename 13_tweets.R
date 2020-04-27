rm(list = ls())

load("data/tweets_for_replication.Rdata")

# How many times did MPs tweet about petitions?

strings_to_search <- "petition"
table(grepl(strings_to_search,all_tweets$urls_expanded, fixed = F))

# How many MPs tweeted about petitions?

length(unique(all_tweets$screen_name[grepl(strings_to_search,all_tweets$urls_expanded, fixed = F)  ]))

# How many times did MPs tweet about petitions?

strings_to_search <- "https://petition.parliament.uk/petitions/"

table(grepl(strings_to_search,all_tweets$urls_expanded, fixed = F))

# How many MPs tweeted about petitions?

length(unique(all_tweets$screen_name[grepl(strings_to_search,all_tweets$urls_expanded, fixed = F)  ]))

