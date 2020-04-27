# "Online Activism and Dyadic Representation" replication files

This archive includes the data and scripts necessary to reproduce all the empirical analyses in "Online Activism and Dyadic Representation: Evidence from the UK E-Petition System" by Jack Blumenau, Legislative Studies Quarterly (Forthcoming). A preprint of the paper can be found [here](https://www.jackblumenau.com/papers/petitions.pdf).

All the analyses for the published version of the paper were completed in R version 3.6.2, using RStudio (version 1.2.5033), under macOS Sierra 10.12.6. The versions of various R packages are indicated in the scripts in which they are used. The machine used to calculate the timings of the various scripts (shown in 00_master.R) had a 3.8 Ghz processor, and 16 GB RAM.

If you wish to recreate the analysis in the paper, you should begin by downloading this repository.

## Scripts

There are a number of scripts that need to be run in sequence in order to recreate the analyses. The 00_master.R script can be executed (in R) to run all the scripts in order, but be warned that it will take approximately 30 hours to complete everything.

The master script will also create the necessary file structure to store the various outputs (plots, tables, etc) which link up to the latex file for the working paper. Even if you do not execute the master file in one go, I recommend running lines 1 to 24 of the master script before executing the other scripts so that you have the correct file structure on your local machine.

* 00_master.R
* 01_data_prep.R
* 02_topic_run.R
* 03_model_prep.R
* 04_models_run.R
* 05_models_run_MCMC.R
* 06_analysis.R
* 07_prep_approval.R
* 08_approval_model_prep.R
* 09_approval_run.R
* 10_approval_analysis.R
* 11_petition_analysis.R
* 12_topic_analysis.R
* utils.R

## Models

In addition to the scripts that prepare the data and run the various models, there are also scripts used to define the various models used in the paper in Stan. These are all kept in the `/stancode` directory. These scripts are called by `03_model_prep.R` and `08_approval_model_prep.R`.

## Data

The code in this repository relies on a number of data files, which are stored in a directory entitled `/data`. (If you are accessing this repository via GitHub, you will need to download the data files from the LSQ dataverse here) The raw data files are compiled in `01_data_prep.R` and other places. The main two files at the core of the analysis are:

* `debates_for_replication.Rdata`

    - Contains an R data.frame object named `debates` which includes all debates (including raw texts) conducted in the House of Commons or Westminster hall between May 2015 and February 2019
    - This data was compiled from the XML files made available at [ParlParse](http://parser.theyworkforyou.com/hansard.html)

* `petitions_for_replication.Rdata`
  
    - Contains an R list object named `petition_signatures_by_constituency_list` which includes one data.frame per petition, each of which includes the number of signatures to that petition from a given constituency
    - Contains an R data.frame names `petitions_meta` which includes various meta information about the e-petitions included in the above list
    - This data was scraped from the [UK Parliament's e-petition website](https://petition.parliament.uk)

Other data files included:

* `BES-2015-General-Election-results-file-v2.2.dta` and `BES-2017-General-Election-results-file-v1.0.dta` -- results from the 2015 and 2017 UK general election, as recorded by the [British Election Study](https://www.britishelectionstudy.com/data-objects/linked-data/)
* `constituency_votereg.csv` -- constituency electorate totals, as recorded by the [House of Commons Library](https://commonslibrary.parliament.uk/parliament-and-elections/elections-elections/uk-elections/constituency-data-electorates/)
* `frontbenchers.Rdata` -- contains `ministers` and `shadow_ministers` which give information on the MPs who help positions in either government or opposition leadership roles. Data taken from the parliamentary [Members' Names Data Platform](http://data.parliament.uk/membersdataplatform/memberquery.aspx)
* `mps_start_end_for_replication.Rdata` -- provides start and end dates for MPs' tenure in the Commons. Data taken from the parliamentary [Members' Names Data Platform](http://data.parliament.uk/membersdataplatform/memberquery.aspx)
* `petition_committee_members.xml` -- provides information of MPs who served on the parliamentary Petitions Committee. Data taken from the parliamentary [Members' Names Data Platform](http://data.parliament.uk/membersdataplatform/memberquery.aspx)
* `petition_to_twfy_mapping.csv` - manually constructed `csv` file which links petitions to their relevant debates
* `/agreement_codings` - manually annotated petition speeches coded by two coders
* `annual_income.csv`, `constituency_votereg.csv`, `population_2016.csv` include constituency-level information taken from the Office for National Statistics
* `Hanretty-Constituency-Estimates.csv` -- Chris Hanretty's estimates of constituency-level 2016 EU referendum outcomes
* `tweets_for_replication.Rdata` -- Tweets sent by MPs during the study period, collected using the `twittR` app

## Latex

The latex directory includes the source code for the working paper, including the .bib bibliography file. 

* `lsq_publication_submission.tex`
* `petition_bib.bib`


  