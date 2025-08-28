
library(tidyverse)
library(readr)
UWOYT_yield_rust_damage <- read_csv("data/UWOYT_yield,rust,damage.csv")
colnames(UWOYT_yield_rust_damage)

data <- UWOYT_yield_rust_damage %>% 
  select(studyName,germplasmName,observationUnitName, blockNumber,replicate,`Grain yield - g/m2|CO_350:0000260`,
         `Crown rust severity (flag leaf) - percent|CO_350:0005029`,`Freeze damage severity - 0-9 Rating|CO_350:0005001`,`Winter stress severity - 0-9 Rating|CO_350:0005003`) %>% 
  rename(freeze_damage = `Freeze damage severity - 0-9 Rating|CO_350:0005001`) %>% 
  rename(winter_stress = `Winter stress severity - 0-9 Rating|CO_350:0005003`) %>%
  rename(rust = `Crown rust severity (flag leaf) - percent|CO_350:0005029`) %>% 
  rename(grain_gm2 = `Grain yield - g/m2|CO_350:0000260`) %>% 
  filter(!is.na(grain_gm2))



data %>% 
  filter(!is.na(rust)) %>% 
  
  ggplot(aes(rust,grain_gm2))+
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  xlab("Rust")+
  ylab("Yield")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))


rust_data <- data %>% 
  filter(!is.na(rust))

lm_yield_rust <- lm(grain_gm2 ~ (rust + studyName), data = rust_data)

lm_yield_rust

EV= -1.25*(1/7)


data %>% 
  filter(!is.na(winter_stress)) %>% 
  
  ggplot(aes(winter_stress,grain_gm2))+
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  xlab("winter_stress")+
  ylab("Yield")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))






data %>% 
  filter(!is.na(rust))

