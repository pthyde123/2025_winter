
library(dplyr)
library(readr)
library(BGLR)

library(corrplot)

X2025_winter_crossing_phenotypes <- read_csv("data/2025_winter_crossing_phenotypes.csv")

colnames(X2025_winter_crossing_phenotypes)


cross_phenotypes <- X2025_winter_crossing_phenotypes %>% 

  mutate(intercrop = if_else(`Cornell_WinterOatPeaIntercrop_2024_Ithaca_pea_genotype_Blaze`== 1 
                             | intercropGermplasmName== "BLAZE","inter","mono")) %>%
  
  mutate(intercrop = if_else(is.na(intercrop),"mono",intercrop)) %>% 

  select(studyName,blockNumber,observationLevel,germplasmName,intercrop,`Freeze damage severity - 0-9 Rating|CO_350:0005001`,
         `Grain yield - g/m2|CO_350:0000260`
         ) %>% 
  filter(observationLevel == "plot") %>% 
  mutate(exp_block = str_c(studyName,"_",blockNumber)) %>% 
  
  print(n=94)



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


df <- selection_index %>% 
  as.data.frame() %>% 
  mutate(germplasmName = str_remove(row.names(selection_index),"germplasmName")) %>% 
  mutate(adj_freeze_SI = (-1 * `Freeze damage severity - 0-9 Rating|CO_350:0005001` * 10)) %>% 
  mutate(yield_SI  =  `Grain yield - g/m2|CO_350:0000260`) 

df2 <- df %>% 
  select(adj_freeze_SI,yield_SI)

row.names(df2) <- df$germplasmName

df %>% 
  mutate(winter_rank = rank(-df$adj_freeze_SI)) %>% 
  mutate(grain_rank = rank(-df$yield_SI)) %>% 
  select(germplasmName,grain_rank,winter_rank) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE, col.names=TRUE)



df %>% 
  mutate(winter_rank = rank(-df$adj_freeze_SI)) %>% 
  mutate(grain_rank = rank(-df$yield_SI)) %>% 
  select(germplasmName,adj_freeze_SI,yield_SI) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE, col.names=TRUE)






