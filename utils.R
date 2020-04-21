##### ####################################################
#####                                               ######
#####             Commonly used functions             
#####                                               ######
##### ####################################################

trim <- function(s) gsub("^[[:space:]]+|[[:space:]]+$","",s)
word.count <- function(str1) sapply(gregexpr("\\W+", str1), length) + 1
