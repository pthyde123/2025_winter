

library(tidyverse)
library(readxl)
library(readr)


#####  Winter Oat Founders crossing  ####


#### Import data ####

# Genotyped winter oat accessions
OatAccessionsAvailableFromUSDA_Ithaca <- read_excel("data/OatAccessionsAvailableFromUSDA_Ithaca.xlsx")  # this is a list of everything we have genotyped

# meta data used for synonyms, and population ID, lots of other info here
WOF_meta <- read_excel("data/WOF_meta.xlsx")  # meta data used for synonyms, and population ID, lots of other info here

# synonyms of Nobel lines from Lauren
Nobel_Lauren_line_synonyms <- read_csv("data/Nobel_Lauren_line_synonyms.csv")

# phenotype data from the WOF headrow trial
Cornell_WinterOatFounders_2024_Headrow_phenotypes <- read_excel("data/Cornell_WinterOatFounders_2024_Headrow_phenotypes.xlsx")


# BLUP and selection index from 2024 winter oat plot trial
OatTraitBLUPs_inSel <- read_csv("data/OatTraitBLUPs_inSel.csv")



#### setup the data ####
# mostly assigning proper t3 accession names and phenotype data

df <- OatAccessionsAvailableFromUSDA_Ithaca %>% 
  filter(`Sample Group` != "spring-2023-oat-pea") %>%  # remove spring oats 
  mutate("source" = substr(`Sample Name`, start = 1, stop = 2)) # the first 2 letters of the sample name indicate where each accession is from, this will be used to determine if we move forward with the accesssion


unique(df$source)  # List of the source we will send "NF","PI","CI","Cl","Au" accessions. These are from the Noble Foundation, or GRIN, AAFC Winnipeg.  
  #  I think we only want to do crosses with WOF lines, to bad the NC lines are very good 

# make a list of genotypes we can work with
accessions_available <- OatAccessionsAvailableFromUSDA_Ithaca %>% 
  filter(`Sample Group` != "spring-2023-oat-pea") %>% 
  mutate("source" = substr(`Sample Name`, start = 1, stop = 2)) %>% 
  filter(source %in% c("NF","PI","CI","Cl","Au"))  ## selects only GRIN and Nobel lines we can work with
  

# make a table to accessions (proper T3 names) and synonyms used elsewhere
accession_synonyms <- WOF_meta %>% 
  select(accession,PI_GRIN) %>% 
  mutate("name" = str_remove(PI_GRIN," ")) %>% 
  rename(synonym = name) %>% 
  select(accession, synonym) %>% 
  bind_rows(Nobel_Lauren_line_synonyms)


# add add proper accession names to genotyped accession list
accessions_crossable <- accessions_available %>% 
  left_join(accession_synonyms,by=join_by(`Sample Name`== synonym)) %>% 
  #mutate(accession = if_else(is.na(accession),`Sample Name`,accession)) %>% 
  select(Index:`Notes for Ottawa`,accession) 


# add in selection index
OatTraitBLUPs_inSel 

accession_selection_index <- OatTraitBLUPs_inSel %>% 
        select(Oat,SelIndex) %>% 
        left_join(accession_synonyms, join_by(Oat == synonym)) %>% 
        mutate(accession = if_else(is.na(accession),Oat,accession)) 


# add in headrow phenotypes data
WOF_headrow_data <- Cornell_WinterOatFounders_2024_Headrow_phenotypes %>% 
  select(germplasmName,`Grain weight - g|CO_350:0005123`,`Heading - %|CO_350:0005127`) %>% 
  rename(accession = germplasmName)




cross_selections <- accessions_crossable %>% 
  left_join(WOF_meta) %>% 
  left_join(accession_selection_index) %>% 
  select(accession,Prob_P21,SelIndex,`GROWTH HABIT`,LODGING,`Sample Group`,YIELD,`1000 KERNEL WEIGHT`,ORIGIN_GRIN) %>%  # can add in more in needed
  left_join(WOF_headrow_data) %>%
  arrange(`Sample Group`,desc(SelIndex),`Grain weight - g|CO_350:0005123`)

# all the combined data and t3 accession name for making cross selections
cross_selections %>% 
  print(n = nrow(cross_selections))    



#### make cross selections ####



#Narrow it down a bit
#Plot selection index is greater than 10. Use these 6
#Headrow plots that headed indicating they do not need to be vernalized

cross_selections %>% 
  arrange(`Sample Group`,desc(SelIndex),`Grain weight - g|CO_350:0005123`) %>% 
  filter(SelIndex > 10 | is.na(SelIndex))%>% 
  filter(`Heading - %|CO_350:0005127` > 80 | is.na(`Heading - %|CO_350:0005127`) ) %>% 
  #arrange(Prob_P21) %>% 
  print(n= nrow(accessions_crossable))


## Manually selected the ones I want, based on the output above
## Three groups, six accession in each

# 6 best from the 2024 winter oat plot experiment based on selection index
plot_best <- c("NF13-4126-4_3","AURORA","NF01404A","NF12AS-107-4_4","NF12AS-108-4_1","NF97405B2")

# 6 winter types that appear to not need vernalization 
# my idea is that these should have good winter heartiness, some are from NY and PA 
winter_type <- c("PA8014-840","AWNLESS_CURLED","WINTER_TURF|CIAV996","SEGETAL","TRISPERNIA|CIAV5100","WESTFINNISCHER_SCHWARZ")

# 6 best from GRIN based on headrow seed produced and likely hood in P21 
# "CAV3163" is very similar to CAV3088 so I replaced it, with the next best PI365616.
headrow_best <- c("PI344827","PI365619","AVE265_59","PI365622","CAV3088","PI365616")   

headrow_pop21 <- c("CW559","PI344818","AVE265_59","KARCAGI","SEGER|PI306405","DOMACA_ZOB")



selections <- cross_selections %>% 
  left_join(WOF_meta) %>% 
  left_join(accession_selection_index) %>% 
  select(accession,Prob_P21,SelIndex,`GROWTH HABIT`,LODGING,`Sample Group`,YIELD,`1000 KERNEL WEIGHT`,ORIGIN_GRIN) %>%
  left_join(WOF_headrow_data) %>%
  filter(accession %in% plot_best | accession %in% winter_type | accession %in% headrow_pop21 )

###  add in a few more from NC 

NC_add <- c("Gerard 227","NC20-4526","NC20-4452","NC21-6429")

NC_add <- accession_selection_index %>% 
  select(accession,SelIndex) %>% 
  filter(accession %in% c("Gerard 227","NC20-4526","NC20-4452","NC21-6429"))

selections_2 <- bind_rows(NC_add,selections)


##write.csv(selections_2,'output/2025_winter_oat_crosses.csv',row.names = F)







