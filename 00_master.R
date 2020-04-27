##### #####################################################################
#####           Online Activism and Dyadic Representation                   
#####             Evidence from the UK E-Petition System                    
#####                         Jack Blumenau                                 
#####                  Legislative Studies Quarterly                        
#####                         Master script                                 


rm(list= ls())

# Create directory structure

directories <- c("working",
                 "working/topicmodel",
                 "working/stanOut",
                 "working/stanOut/robustness",
                 "working/stanOut/approval",
                 "latex/tables",
                 "latex/useful_numbers",
                 "latex/figures",
                 "latex/closest_debates",
                 "timings")

lapply(directories, function(x) dir.create(x))

# Data prep

system("(time Rscript 01_data_prep.R 2> tmp.txt;) 2> timings/01_data_prep.txt") # 2 mins
system("(time Rscript 02_topic_run.R 2> tmp.txt;) 2> timings/02_topic_run.txt") # 78 mins
system("(time Rscript 03_model_prep.R 2> tmp.txt;) 2> timings/02_model_prep.txt") # 6 mins
system("(time Rscript 04_models_run.R 2> tmp.txt;) 2> timings/04_models_run.txt") # ~ 30 hours
system("(time Rscript 05_models_run_MCMC.R 2> tmp.txt;) 2> timings/05_models_run_MCMC.txt") # 
system("(time Rscript 06_analysis.R 2> tmp.txt;) 2> timings/06_analysis.txt") # 3 mins
system("(time Rscript 07_prep_approval.R 2> tmp.txt;) 2> timings/07_prep_approval.txt") # <1 min 
system("(time Rscript 08_approval_model_prep.R 2> tmp.txt;) 2> timings/08_approval_model_prep.txt") # 3 mins
system("(time Rscript 09_approval_run.R 2> tmp.txt;) 2> timings/09_approval_run.txt") # <1 min
system("(time Rscript 10_approval_analysis.R 2> tmp.txt;) 2> timings/10_approval_analysis.txt") # <1 min 
system("(time Rscript 11_petition_analysis.R 2> tmp.txt;) 2> timings/11_petition_analysis.txt") # <1 min
system("(time Rscript 12_topic_analysis.R 2> tmp.txt;) 2> timings/12_topic_analysis.txt") # <1 min 
system("(time Rscript 13_tweets.R 2> tmp.txt;) 2> timings/13_tweets.txt") # <1 min
