

library(readxl)
library(AlgDesign)
library(tidyverse)
library(readxl)


floor(runif(1, 10000, 1000000))  # run to get a random number for set.seed
set.seed(892015) 

# import entry list
input_file <- read_excel("data/2025_SOAP_Entries.xlsx") %>% 
  mutate(accession = Name) #### IDENTIFY ACCESSIONS


# create accession list
accessions <- input_file %>% 
  select(accession) %>% 
  distinct()  # ensure accession list has no duplicates

nBlks <- 2   # set the block number

nAccessions <- nrow(accessions)  # how many accessions in the plan

nPlotsPerBlk <- nAccessions # set the number of plots per block


blocks <- dplyr::tibble( 
  accession=rep(accessions$accession, nBlks),
  block= rep(1:nBlks,  each = nPlotsPerBlk ) )


set.seed(892015) 
randomization  <- blocks %>% 
  left_join(input_file, join_by(accession)) %>% ### add in metadata from input file
  mutate("random" = runif(1:nrow(blocks))) %>% 
  arrange(block,random) %>%   # order by random numbers
  mutate(plot = seq(1:nrow(blocks))) %>%  # add in sequential plot numbers
  relocate(block,plot,accession)


write.csv(randomization,"output/SOAP_randomization.csv",row.names = FALSE)

