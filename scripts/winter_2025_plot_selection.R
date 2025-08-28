library(tidyverse)
library(readxl)
library("readr")  



CU_2025_Ithaca_WOP_PLOT_phenotypes <- read_csv("data/CU_2025_Ithaca_WOP_PLOT_phenotypes.csv")


spectral_4_04_25_data <- read_csv("output/spectral_4-04-25_data.csv")


colnames(CU_2025_Ithaca_WOP_PLOT_phenotypes)




plot_spectral <- spectral_4_04_25_data %>% 
  select(observationUnitName,plot_number,`ExG_0-1_mean`,NDVI_mean) %>%
  group_by(plot_number) %>% 
  summarise_at(c("ExG_0-1_mean", "NDVI_mean"), mean, na.rm = F) 


plot_spectral



df<-CU_2025_Ithaca_WOP_PLOT_phenotypes %>% 
  select(observationUnitName,observationLevel,plotNumber,germplasmName,intercropGermplasmName,
         `Freeze damage severity - 0-9 Rating|CO_350:0005001`,
         `Winter survival - percent|CO_350:0000170`,
         "Pea winter survival - percent|COMP:0000050",
         "Pea freeze damage severity - 0-5 Rating|COMP:0000051") %>% 
  filter(observationLevel == "plot") %>% 
  left_join(plot_spectral, join_by(plotNumber==plot_number)) %>% 
  mutate(winter_hardiness = ((10-`Freeze damage severity - 0-9 Rating|CO_350:0005001`)*10) * `Winter survival - percent|CO_350:0000170`) %>% 
  mutate(intercrop = if_else(is.na(intercropGermplasmName),"mono","inter")) 
  

library(RColorBrewer)

df %>% 
  ggplot(aes(`Freeze damage severity - 0-9 Rating|CO_350:0005001`,`NDVI_mean`,color=intercrop))+
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  xlab("Freeze Damage")+
  ylab("`NDVI`")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))


df %>% 
  ggplot(aes(`Winter survival - percent|CO_350:0000170`,`ExG_0-1_mean`,color=intercrop))+
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  xlab("Freeze Damage")+
  ylab("`ExG_0-1_mean`")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))


df <-CU_2025_Ithaca_WOP_PLOT_phenotypes %>% 
  select(observationUnitName,observationLevel,plotNumber,germplasmName,intercropGermplasmName,
         `Freeze damage severity - 0-9 Rating|CO_350:0005001`,
         `Winter survival - percent|CO_350:0000170`,
         "Pea winter survival - percent|COMP:0000050",
         "Pea freeze damage severity - 0-5 Rating|COMP:0000051") %>% 
  filter(observationLevel == "plot") %>% 
  left_join(plot_spectral, join_by(plotNumber==plot_number)) %>% 
  mutate(winter_hardiness = ((10-`Freeze damage severity - 0-9 Rating|CO_350:0005001`)*10) * `Winter survival - percent|CO_350:0000170`) %>% 
  mutate(intercrop = if_else(is.na(intercropGermplasmName),"mono","inter")) 


hist(df$`Winter survival - percent|CO_350:0000170`)

hist(df$`Freeze damage severity - 0-9 Rating|CO_350:0005001`)


df2<-CU_2025_Ithaca_WOP_PLOT_phenotypes %>% 
  select(observationUnitName,observationLevel,plotNumber,germplasmName,intercropGermplasmName,
         `Freeze damage severity - 0-9 Rating|CO_350:0005001`,
         `Winter survival - percent|CO_350:0000170`,
         "Pea winter survival - percent|COMP:0000050",
         "Pea freeze damage severity - 0-5 Rating|COMP:0000051") %>% 
  filter(observationLevel == "plot") %>% 
  left_join(plot_spectral, join_by(plotNumber==plot_number)) %>% 
  mutate(winter_hardiness = ((10-`Freeze damage severity - 0-9 Rating|CO_350:0005001`)*10) * `Winter survival - percent|CO_350:0000170`) %>% 
  mutate(intercrop = if_else(is.na(intercropGermplasmName),"mono","inter")) %>% 
  filter(`Winter survival - percent|CO_350:0000170` > 70) %>% 
  filter(`Freeze damage severity - 0-9 Rating|CO_350:0005001` < 8) %>% 
  arrange(`Winter survival - percent|CO_350:0000170`)



df2 %>% 
  select(observationUnitName,germplasmName) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)



CU_2025_Ithaca_WOP_PLOT_phenotypes %>% 
  select(observationUnitName,observationLevel,plotNumber,germplasmName,intercropGermplasmName,
         `Freeze damage severity - 0-9 Rating|CO_350:0005001`,
         `Winter survival - percent|CO_350:0000170`,
         "Pea winter survival - percent|COMP:0000050",
         "Pea freeze damage severity - 0-5 Rating|COMP:0000051") %>% 
  filter(observationLevel == "plot") %>% 
  left_join(plot_spectral, join_by(plotNumber==plot_number)) %>% 
  mutate(winter_hardiness = ((10-`Freeze damage severity - 0-9 Rating|CO_350:0005001`)*10) * `Winter survival - percent|CO_350:0000170`) %>% 
  mutate(intercrop = if_else(is.na(intercropGermplasmName),"mono","inter")) %>% 
 filter(germplasmName == "AURORA") %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)



x <- CU_2025_Ithaca_WOP_PLOT_phenotypes %>% 
  filter(germplasmName == "NC20-4452")

x <- CU_2025_Ithaca_WOP_PLOT_phenotypes %>% 
  filter(germplasmName == "NC21-6610")

