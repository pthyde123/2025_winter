
library(tidyverse)
library(readxl)

Cornell_WinterOatFounders_2024_Headrow_phenotypes <- read_excel("data/Cornell_WinterOatFounders_2024_Headrow_phenotypes.xlsx")

WOF_meta <- read_excel("data/WOF_meta.xlsx")


colnames(Cornell_WinterOatFounders_2024_Headrow_phenotypes)

colnames(WOF_meta)


Cornell_WinterOatFounders_2024_Headrow_phenotypes %>%
  select("germplasmName","observationUnitName","Heading - %|CO_350:0005127","Grain weight - g|CO_350:0005123") %>% 
  left_join(WOF_meta %>% 
              select(accession,ORIGIN,NARRATIVE,LODGING,"GROWTH HABIT","DAYS TO ANTHESIS","STRAW BREAKAGE","YIELD","IMPROVEMENT STATUS",PI_GRIN,Site,list,latitude,longitude,),join_by(germplasmName==accession)) %>% 
  print(n=nrow(Cornell_WinterOatFounders_2024_Headrow_phenotypes)) %>% 
  write.csv( "output/WOF_seed.csv", row.names = F)
  
### opened WOF_seed in excel and manually made selections based on seed available, heading percent, narrative, unique names, winter types, locations saved it as WOF_headrow_seed for randomization
### do this with selection index in the future

## randomizations
library(readr)
WOF_headrow_seed <- read_csv("output/WOF_headrow_seed.csv")



####  Plot Increase

floor(runif(1, 10000, 1000000))  # run to get a random number for set.seed
set.seed(859783) 

# import entry list
input_file <- WOF_headrow_seed %>% 
  select(observationUnitName, germplasmName,use, nReps,`Grain weight - g|CO_350:0005123`) %>% 
  filter(!is.na(nReps)) %>% 
  mutate(accession = germplasmName) %>% #### IDENTIFY ACCESSIONS
  filter(use=="plot increase")

# create accession list
accessions <- input_file %>% 
  select(accession) %>% 
  distinct()  # ensure accession list has no duplicates

nBlks <- 1   # set the block number

nAccessions <- nrow(accessions)  # how many accessions in the plan

nPlotsPerBlk <- nAccessions # set the number of plots per block

trial_name <- "plot_increase"  # name the trial for csv name

blocks <- dplyr::tibble( 
  accession=rep(accessions$accession, nBlks),
  block= rep(1:nBlks,  each = nPlotsPerBlk ) )


set.seed(859783) 
randomization  <- blocks %>% 
  left_join(input_file, join_by(accession)) %>% ### add in metadata from input file
  mutate("random" = runif(1:nrow(blocks))) %>% 
  arrange(block,random) %>%   # order by random numbers
  mutate(plot = seq(1:nrow(blocks))) %>%  # add in sequential plot numbers
  relocate(block,plot,accession)


write.csv(randomization,paste("output/",trial_name,"_randomization.csv"),row.names = FALSE)





####  Headrow Increase



floor(runif(1, 10000, 1000000))  # run to get a random number for set.seed
set.seed(829171) 

# import entry list
input_file <- WOF_headrow_seed %>% 
  select(observationUnitName, germplasmName,use, nReps,`Grain weight - g|CO_350:0005123`) %>% 
  filter(!is.na(nReps)) %>% 
  mutate(accession = germplasmName) %>% #### IDENTIFY ACCESSIONS
  filter(use=="headrow")

# create accession list
accessions <- input_file %>% 
  select(accession) %>% 
  distinct()  # ensure accession list has no duplicates

nBlks <- 2   # set the block number

nAccessions <- nrow(accessions)  # how many accessions in the plan

nPlotsPerBlk <- nAccessions # set the number of plots per block

trial_name <- "headrow_increase"  # name the trial for csv name

blocks <- dplyr::tibble( 
  accession=rep(accessions$accession, nBlks),
  block= rep(1:nBlks,  each = nPlotsPerBlk ) )


set.seed(829171) 
randomization  <- blocks %>% 
  left_join(input_file, join_by(accession)) %>% ### add in metadata from input file
  mutate("random" = runif(1:nrow(blocks))) %>% 
  arrange(block,random) %>%   # order by random numbers
  mutate(plot = seq(1:nrow(blocks))) %>%  # add in sequential plot numbers
  relocate(block,plot,accession)

###add in code to sort by seed pack


write.csv(randomization,paste("output/",trial_name,"_randomization.csv"),row.names = FALSE)


