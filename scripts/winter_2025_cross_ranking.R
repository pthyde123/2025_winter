
library(dplyr)
library(readr)
library(BGLR)
library(tidyverse)
library(corrplot)
library(readxl)

X2025_winter_crossing_phenotypes <- read_csv("data/2025_winter_crossing_phenotypes.csv")  # combined T3 Data with 3 trials, 
  # Cornell_WinterOatPeaIntercrop_2024_Ithaca, CU_2025_Ithaca_WOF_HR, CU_2025_Ithaca_WOF_PLOT, CU_2025_Ithaca_WOP_PLOT
  # only including data from accession used in crosses, did the crosses first and now selecting. 


WOF_meta <- read_excel("data/WOF_meta.xlsx")  ## extra info on WOF from GRIN, used to ID POP21 lines


colnames(X2025_winter_crossing_phenotypes)

# formatting the phenotype data, only using Freeze damage severity and Yield. 
cross_phenotypes <- X2025_winter_crossing_phenotypes %>% 

  mutate(intercrop = if_else(`Cornell_WinterOatPeaIntercrop_2024_Ithaca_pea_genotype_Blaze`== 1 
                             | intercropGermplasmName== "BLAZE","inter","mono")) %>%
  
  mutate(intercrop = if_else(is.na(intercrop),"mono",intercrop)) %>% 

  select(studyName,blockNumber,observationLevel,germplasmName,intercrop,`Freeze damage severity - 0-9 Rating|CO_350:0005001`,
         `Grain yield - g/m2|CO_350:0000260`
         ) %>% 
  filter(observationLevel == "plot") %>% 
  mutate(exp_block = str_c(studyName,"_",blockNumber)) %>% 
  
  mutate(`Freeze damage severity - 0-9 Rating|CO_350:0005001` = if_else(germplasmName == "NF13-4126-4_3" & studyName == "CU_2025_Ithaca_WOP_PLOT", NA, 
                                                                        `Freeze damage severity - 0-9 Rating|CO_350:0005001` )) %>% 
  print(n = 94)



### BGLR to get yield and freeze damage selection index for each accession used for crossing

yTraits <- as.matrix(dplyr::select(cross_phenotypes, contains("Yield"),contains("damage")))
incLocations <- model.matrix(~ -1 + studyName, cross_phenotypes)
incBlocks <- model.matrix(~ -1 + exp_block, cross_phenotypes)
incOatAcc <- model.matrix(~ -1 + germplasmName, cross_phenotypes)
incIntercrop <- model.matrix(~ -1 + intercrop, cross_phenotypes)


ETA <- list(list(X=incLocations, model="FIXED"),
            list(X=incBlocks, model="BRR"),
            list(X=incOatAcc, model="BRR"),
            list(X=incIntercrop, model="FIXED"))


tst2 <- BGLR::Multitrait(yTraits, ETA, intercept=TRUE,
                         resCov=list(df0=4,S0=NULL,type="UN"),
                         R2=0.5,
                         nIter=1000, burnIn=200,
                         thin=10, saveAt="",verbose=FALSE)

oatEff <- tst2$ETA[[3]]$beta

oatEffSD <- tst2$ETA[[3]]$SD.beta

fitOE <- lm(oatEff[,1] ~ oatEff[,2])

oatEffCov <- tst2$ETA[[3]]$Cov$Omega

plot(oatEff)

selection_index  <- oatEff 

row.names(selection_index) <- colnames(incOatAcc)
colnames(selection_index) <- colnames(yTraits)


selection_index

#### calculate cross selection index by adding the freeze and yield selection index
 
df <- selection_index %>% 
  as.data.frame() %>% 
  mutate(germplasmName = str_remove(row.names(selection_index),"germplasmName")) %>% 
  mutate(adj_freeze_SI = (-1 * `Freeze damage severity - 0-9 Rating|CO_350:0005001` * 10)) %>% ## trying to get them roughly equivalent values and high freeze values as good see plot(oatEff)
  mutate(yield_SI  =  `Grain yield - g/m2|CO_350:0000260`) 

df2 <- df %>% 
  select(adj_freeze_SI,yield_SI)

row.names(df2) <- df$germplasmName

df2 <-df2 %>% 
  mutate(SI_F_p1 = adj_freeze_SI) %>% 
  mutate(SI_Y_p1 = yield_SI) %>% 
  mutate(SI_F_p2 = adj_freeze_SI) %>% 
  mutate(SI_Y_p2 = yield_SI) %>% 
  
  mutate(p1 = row.names(df2)) %>% 
  mutate(p2 = row.names(df2)) 


p1 <- df2 %>% 
  select(p1,SI_F_p1,SI_Y_p1) 

p2 <- df2 %>% 
  select(p2,SI_Y_p2,SI_F_p2)
  
cross <- expand.grid(p1 = df2$p1, p2 = df2$p2)  #create a list of all possible cross combos

cross_SI <- cross %>% 
  left_join(p1, by = join_by(p1 == p1)) %>% # left join filters down to only crosses we have
  left_join(p2, by = join_by(p2 == p2)) %>% # left join filters down to only crosses we have
  
  mutate(cross_SI = SI_F_p1 + SI_Y_p1 + SI_F_p2 + SI_Y_p2) %>% 
  arrange(-cross_SI) %>% 
  mutate(cross = str_c(p1," X ", p2)) %>% 
  distinct(cross, .keep_all = TRUE) %>% 
  filter(p1 != p2)

# getting the cross validation rankings previously calculated 
cross_val_score <- read_excel("data/2025_cross_validation_score.xlsx") %>% 
  rename( "p1" =`Female Parent` ) %>% 
  rename( "p2" =`Male Parent` ) %>% 
  select(score,p1,p2) %>% 
  mutate(cross = str_c(p1," X ",p2))

# combine cross SI, cross_val and WOF_meta
cross_selections <- cross_val_score %>% 
  left_join(cross_SI) %>% 
  arrange(-cross_SI) %>% 
  
  left_join(WOF_meta %>% 
              select(accession,list), 
            join_by(p1==accession)) %>% 
  rename(p1_list = list) %>% 
  
  left_join(WOF_meta %>% 
              select(accession,list), 
            join_by(p2==accession)) %>% 
  rename(p2_list = list) %>% 
  
  rename(cross_val_score = score) %>% 
  
  select(p1, p2, cross, SI_F_p1, SI_Y_p1, SI_F_p2, SI_Y_p2,cross_SI, cross_val_score, p1_list, p2_list  )



cross_selections

# write.csv(cross_selections, "output/cross_selections_2025.csv")




