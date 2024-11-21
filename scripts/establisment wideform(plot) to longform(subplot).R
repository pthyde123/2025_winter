###  Script to convert standcounts from wide form (plot) to long form (subplot)


library(tidyverse)
library(readxl)
library(readr)



# We recorded the each subplot as a trait in field book



CU_2025_Ithaca_WOP_PLOT_phenotype_collection_plot_ <- read_excel("data/CU_2025_Ithaca_WOP_PLOT_phenotype_collection (plot).xlsx", 
                                                                 sheet = "full")


establishment_traits <- c("Pea plant establishment - plants/ft2|COMP:0000052", "Plant establishment - plants/ft2|CO_350:0005124")





oat <- CU_2025_Ithaca_WOP_PLOT_phenotype_collection_plot_ %>% 
  select(observationunit_name, `Oat Stand 1`, `Oat Stand 2`, `Oat Stand 3`) %>% 
  pivot_longer(`Oat Stand 1`:`Oat Stand 3`,names_to = "trait", values_to = "Plant establishment - plants/ft2|CO_350:0005124") %>% 
  mutate(subplot = str_sub(trait, -1)) %>% 
  select(observationunit_name,subplot, "Plant establishment - plants/ft2|CO_350:0005124")
  
  
pea <- CU_2025_Ithaca_WOP_PLOT_phenotype_collection_plot_ %>% 
  select(observationunit_name, `Pea Stand 1`, `Pea Stand 2`, `Pea Stand 3`) %>% 
  pivot_longer(`Pea Stand 1`:`Pea Stand 3`,names_to = "trait", values_to = "Pea plant establishment - plants/ft2|COMP:0000052") %>% 
  mutate(subplot = str_sub(trait, -1))  %>% 
  select( "Pea plant establishment - plants/ft2|COMP:0000052")


cbind(oat,pea)



write.csv(cbind(oat,pea), "output/establishment_longform.csv" , row.names=FALSE)

