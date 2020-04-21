##### ##################################################################### ######
#####                                                                       ######
#####           Online Activism and Dyadic Representation                   ######
#####             Evidence from the UK E-Petition System                    ######
#####                         Jack Blumenau                                 ######
#####                  Legislative Studies Quarterly                        ######
#####                         Master script                                 ######
#####                                                                       ######
##### ##################################################################### ######

rm(list= ls())

# Create directory structure

directories <- c("working",
                 "working/topicmodel",
                 "working/stanOut",
                 "working/stanOut/robustness",
                 "out",
                 "out/tables",
                 "out/tables/closest_debates",
                 "out/tables/useful_numbers",
                 "timings")

lapply(directories, function(x) dir.create(x))

# Data prep

system("(time Rscript 01_data_prep.R 2> tmp.txt;) 2> timings/01_data_prep.txt") # 
system("(time Rscript 02_topic_run.R 2> tmp.txt;) 2> timings/02_topic_run.txt") # 78 mins
system("(time Rscript 03_model_prep.R 2> tmp.txt;) 2> timings/02_model_prep.txt") # 6 mins
system("(time Rscript 04_models_run.R 2> tmp.txt;) 2> timings/03_models_run.txt") # 

