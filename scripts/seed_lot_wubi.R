
library(tidyverse)
library(readxl)
library(readr)



######  winter 2024 seedlots

library(readr)
winter_2024_accession_max_seedlots <- read_csv("data/winter_2024_accession_max_seedlots.csv")%>% 
  mutate(seedlot = str_c(germplasmName,"-",observationUnitName)) %>%
  mutate(seed_weight_g = `Grain weight - g|CO_350:0005123`) %>%
  mutate(plotNumber = as.numeric(str_sub(observationUnitName,48))) %>% 
  mutate(trial = str_sub(observationUnitName,1,41)) %>%
  select(observationUnitName,germplasmName,seed_weight_g,seedlot,plotNumber,trial)


library(readxl)
Cornell_WinterOatFounders_2024_Headrow_phenotypes <- read_excel("data/Cornell_WinterOatFounders_2024_Headrow_phenotypes.xlsx") %>% 
  mutate(seed_weight_g = `Grain weight - g|CO_350:0005123`) %>% 
  mutate(seedlot = str_c(str_sub(observationUnitName,1,38),"-",germplasmName,"-1")) %>%
  mutate(plotNumber = as.numeric(str_sub(observationUnitName,40))) %>% 
  mutate(trial = str_sub(observationUnitName,1,38)) %>%
  select(observationUnitName,germplasmName,seed_weight_g,seedlot,plotNumber,trial)

seed_lots <- Cornell_WinterOatFounders_2024_Headrow_phenotypes %>% 
  bind_rows(winter_2024_accession_max_seedlots) 




###  Ithaca 2025 trial seed needed and used


plot_2025 <- read_excel("data/planning.xlsx", 
                       sheet = "selections")  %>% 
  mutate(trial = "plot_2025") %>% 
  mutate(seed_needed_g = plots_to_plant * 40.5) %>% 
  select(trial,germplasmName, plots_to_plant, seedlot,seed_needed_g)



guardrows <- read_excel("data/planning.xlsx", 
                       sheet = "guardrows")%>%
  mutate(plots_to_plant =1) %>% 
  mutate(seed_needed_g = plots_to_plant * 40.5) %>% 
  mutate(seedlot = str_c(germplasmName,"-",observationUnitName)) %>% 
  mutate(seed_used = plots_to_plant * 40.5) %>% 
  mutate(trial = "guardrows") %>% 
  select(trial,germplasmName, plots_to_plant, seedlot,seed_needed_g)



plot_increase <- read_csv("output/ plot_increase _randomization.csv") %>% 
  mutate(plots_to_plant =1) %>% 
  mutate(seedlot = str_c(substr(observationUnitName, start = 1, stop = 38),"-",germplasmName,"-1")) %>% 
  mutate(seed_needed_g = plots_to_plant * 40.5) %>% 
  mutate(trial = "plot_increase") %>% 
  select(trial,germplasmName, plots_to_plant, seedlot,seed_needed_g)



headrow_increase <- read_csv("output/ headrow_increase _randomization.csv") %>% 
  mutate(plots_to_plant =2) %>% 
  mutate(seed_needed_g = plots_to_plant * 3) %>% 
  mutate(seedlot = str_c(substr(observationUnitName, start = 1, stop = 38),"-",germplasmName,"-1")) %>% 
  mutate(trial = "headrow_increase") %>% 
  select(trial,germplasmName, plots_to_plant, seedlot,seed_needed_g)



plot_2025
guardrows
plot_increase
headrow_increase


winter_2025_seed_used <- bind_rows(plot_2025,
          guardrows,
          plot_increase,
          headrow_increase) %>% 
  group_by(seedlot) %>% 
  summarise(seed_used_g = sum(seed_needed_g))


winter_2025_seed_used




##### subtract seed used for winter 2025 planting from original seed lots

seed_lots_10.9.24 <- seed_lots %>% 
  left_join(winter_2025_seed_used, by=join_by(seedlot)) %>% 
  mutate(seed_used_g = if_else(is.na(seed_used_g),0,seed_used_g)) %>% 
  mutate(seed_available = (seed_weight_g - seed_used_g))





#####  seed to sent to Wubi



OatAccessionsAvailableFromUSDA_Ithaca <- read_excel("data/OatAccessionsAvailableFromUSDA_Ithaca.xlsx")  # this is a list of everything we have genotyped

WOF_meta <- read_excel("data/WOF_meta.xlsx")  # meta data used for synonyms

Nobel_Lauren_line_synonyms <- read_csv("data/Nobel_Lauren_line_synonyms.csv")

df <- OatAccessionsAvailableFromUSDA_Ithaca %>% 
  filter(`Sample Group` != "spring-2023-oat-pea") %>%  # remove spring oats 
  mutate("source" = substr(`Sample Name`, start = 1, stop = 2)) # the first 2 letters of the sample name indicate where each accession is from, this will be used to determine if we move forward with the accesssion


unique(df$source)  # List of the source we will send "NF","PI","CI","Cl","Au" accessions. These are from the Noble Foundation, or GRIN, AAFC Winnipeg.  


accessions_to_send <- OatAccessionsAvailableFromUSDA_Ithaca %>% 
  filter(`Sample Group` != "spring-2023-oat-pea") %>% 
  mutate("source" = substr(`Sample Name`, start = 1, stop = 2)) %>% 
  filter(source %in% c("NF","PI","CI","Cl","Au")) %>% 
  print(n=nrow(OatAccessionsAvailableFromUSDA_Ithaca))


accession_synonyms <- WOF_meta %>% 
  select(accession,PI_GRIN) %>% 
  mutate("name" = str_remove(PI_GRIN," ")) %>% 
  select(accession, name) %>% 
  bind_rows(Nobel_Lauren_line_synonyms)



to_send <- accessions_to_send %>% 
  left_join(accession_synonyms,by=join_by(`Sample Name`== name)) %>% 
  mutate(accession = if_else(is.na(accession),`Sample Name`,accession)) %>% 
  select(Index:`Notes for Ottawa`,accession) %>% 
  print(n= nrow(accessions_to_send))




plot_2025
plot_increase
headrow_increase

winter_2025_accessions <- bind_rows(plot_2025,
          guardrows,
          plot_increase,
          headrow_increase)


winter_2025_accessions <- unique(winter_2025_accessions$germplasmName)


winter_2025_accessions <- bind_rows(plot_2025,
          guardrows,
          plot_increase,
          headrow_increase) 


winter_2025_accessions <- unique(winter_2025_accessions)


seed_lots <- Cornell_WinterOatFounders_2024_Headrow_phenotypes %>% 
  bind_rows(winter_2024_accession_max_seedlots) 


seed_for_wubi <- to_send %>% 
  left_join(seed_lots_10.9.24,by=join_by(accession==germplasmName)) %>% 
  select(accession, seedlot, seed_available, observationUnitName,plotNumber,trial) %>%
  filter(accession %in% c(unique(winter_2025_accessions$germplasmName))) %>% 
  arrange(trial,plotNumber) %>% 
  filter(!observationUnitName %in% c("Cornell_WinterOatFounders_2024_Headrow_87","Cornell_WinterOatFounders_2024_Headrow_90")) %>% 
  distinct(accession, .keep_all = TRUE)

  


Cornell_WinterOatFounders_2024_GH_phenotypes <- read_csv("data/Cornell_WinterOatFounders_2024_GH_phenotypes.csv") %>% 
  mutate(GH_seed = `Grain weight - g|CO_350:0005123`) %>% 
  mutate(GH_heading = `Heading - %|CO_350:0005127`) %>% 
  select(observationUnitName,germplasmName, germplasmSynonyms,plotNumber, GH_seed, GH_heading) 


seed_for_wubi <- seed_for_wubi %>% 
  left_join(Cornell_WinterOatFounders_2024_GH_phenotypes, by=join_by(accession == germplasmName)) %>% 
  select(accession,seedlot,observationUnitName.x,plotNumber.x,trial,seed_available,GH_seed.x) %>% 
  mutate("seed_to_send_g" = 8) %>% 
  mutate("seed_left" = seed_available-seed_to_send_g)


### add synonyms that were used on genotyping plate

library(readr)
Nobel_Lauren_line_synonyms <- read_csv("data/Nobel_Lauren_line_synonyms.csv")
library(readxl)
WOF_synonyms <- read_excel("data/WOF_meta.xlsx") %>% 
  select(accession,synonym)

synonyms <- bind_rows(Nobel_Lauren_line_synonyms,WOF_synonyms)

seed_for_wubi <- seed_for_wubi %>% 
  left_join(synonyms)






write.csv(seed_for_wubi,"output/wubi_seed_avaliable.csv",row.names = FALSE)


write.csv(seed_lots,"output/seedlots.csv",row.names = FALSE)

