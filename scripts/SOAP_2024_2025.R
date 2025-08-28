
library(tidyverse)
library(readxl)
library(lme4)
library(readr)


####  OLD CU_2024_2025_SOAP_HR_phenotypes <- read_csv("data/CU_2024_2025_SOAP_HR_phenotypes.csv")
CU_2024_2025_SOAP_HR_phenotypes <- read_csv("data/CU_2025_2024_Ithaca_SOAP_HR.csv")

colnames(CU_2024_2025_SOAP_HR_phenotypes)


data <- CU_2024_2025_SOAP_HR_phenotypes %>% 
  select(studyYear,blockNumber,germplasmName,
         `Crown rust severity (flag leaf) - percent|CO_350:0005029`:
         `Winter survival - percent|CO_350:0000170`) %>% 
  mutate(year_block = str_c(studyYear,"_",blockNumber)) %>% 
  rename(damage = `Freeze damage severity - 0-9 Rating|CO_350:0005001`) %>% 
  rename(survival = `Winter survival - percent|CO_350:0000170`) %>%
  rename(fall_growth = `Growth stage - Zadoks|day 330|COMP:0000095`) %>%
  rename(rust = `Crown rust severity (flag leaf) - percent|CO_350:0005029`) %>% 
  rename(heading = `Heading date - Julian day|CO_350:0000270`) %>% 
  rename(grain_g = `Grain weight - g|CO_350:0005123`) %>% 
  
  
  filter(studyYear != '2024')
  ###filter(!is.na(heading)) 

  #filter(survival > 0)
  
  ### keeping 2024 kills the heritability, I think the flooding was worse than I thought and caused patchiness on more than block 2

### damage

lm_damage <- lme4::lmer(damage ~ (1|germplasmName) + (1|year_block), data = data)

df_damage <- as.data.frame(VarCorr(lm_damage))
vg_damage <- df_damage[df_damage$grp == "germplasmName","vcov"]
ve_damage <- df_damage[df_damage$grp == "Residual", "vcov"]
h2_damage <- vg_damage/(vg_damage + ve_damage/(2))

allBLUPs <- ranef(lm_damage)
germplasmNameBLUPs <- allBLUPs$germplasmName
colnames(germplasmNameBLUPs)[1] <- "damage_BLUPs"

germplasmNameBLUPs$Genotype <- rownames(germplasmNameBLUPs)
germplasmNameBLUPs <- germplasmNameBLUPs[,c(2,1)]
rownames(germplasmNameBLUPs) <- NULL

damage_BLUPs <- germplasmNameBLUPs


##### fall_growth

lm_fall_growth <- lme4::lmer(fall_growth ~ (1|germplasmName) + (1|year_block), data = data)

df_fall_growth <- as.data.frame(VarCorr(lm_fall_growth))
vg_fall_growth <- df_fall_growth[df_fall_growth$grp == "germplasmName","vcov"]
ve_fall_growth <- df_fall_growth[df_fall_growth$grp == "Residual", "vcov"]
h2_fall_growth <- vg_fall_growth/(vg_fall_growth + ve_fall_growth/(2))

allBLUPs <- ranef(lm_fall_growth)
germplasmNameBLUPs <- allBLUPs$germplasmName
colnames(germplasmNameBLUPs)[1] <- "fall_growth_BLUPs"

germplasmNameBLUPs$Genotype <- rownames(germplasmNameBLUPs)
germplasmNameBLUPs <- germplasmNameBLUPs[,c(2,1)]
rownames(germplasmNameBLUPs) <- NULL

fall_growth_BLUPs <- germplasmNameBLUPs


##### survival

lm_survival <- lme4::lmer(survival ~ (1|germplasmName) + (1|year_block), data = data)

df_survival <- as.data.frame(VarCorr(lm_survival))
vg_survival <- df_survival[df_survival$grp == "germplasmName","vcov"]
ve_survival <- df_survival[df_survival$grp == "Residual", "vcov"]
h2_survival <- vg_survival/(vg_survival + ve_survival/(2))

allBLUPs <- ranef(lm_survival)
germplasmNameBLUPs <- allBLUPs$germplasmName
colnames(germplasmNameBLUPs)[1] <- "survival_BLUPs"

germplasmNameBLUPs$Genotype <- rownames(germplasmNameBLUPs)
germplasmNameBLUPs <- germplasmNameBLUPs[,c(2,1)]
rownames(germplasmNameBLUPs) <- NULL

survival_BLUPs <- germplasmNameBLUPs

##### rust

lm_rust <- lme4::lmer(rust ~ (1|germplasmName) + (1|year_block), data = filter(data,!is.na(rust)))

df_rust <- as.data.frame(VarCorr(lm_rust))
vg_rust <- df_rust[df_rust$grp == "germplasmName","vcov"]
ve_rust <- df_rust[df_rust$grp == "Residual", "vcov"]
h2_rust <- vg_rust/(vg_rust + ve_rust/(2))

allBLUPs <- ranef(lm_rust)
germplasmNameBLUPs <- allBLUPs$germplasmName
colnames(germplasmNameBLUPs)[1] <- "rust_BLUPs"

germplasmNameBLUPs$Genotype <- rownames(germplasmNameBLUPs)
germplasmNameBLUPs <- germplasmNameBLUPs[,c(2,1)]
rownames(germplasmNameBLUPs) <- NULL

rust_BLUPs <- germplasmNameBLUPs



##### grain_g

lm_grain_g <- lme4::lmer(grain_g ~ (1|germplasmName) + (1|year_block), data = filter(data,!is.na(grain_g)))

df_grain_g <- as.data.frame(VarCorr(lm_grain_g))
vg_grain_g <- df_grain_g[df_grain_g$grp == "germplasmName","vcov"]
ve_grain_g <- df_grain_g[df_grain_g$grp == "Residual", "vcov"]
h2_grain_g <- vg_grain_g/(vg_grain_g + ve_grain_g/(2))

allBLUPs <- ranef(lm_grain_g)
germplasmNameBLUPs <- allBLUPs$germplasmName
colnames(germplasmNameBLUPs)[1] <- "grain_g_BLUPs"

germplasmNameBLUPs$Genotype <- rownames(germplasmNameBLUPs)
germplasmNameBLUPs <- germplasmNameBLUPs[,c(2,1)]
rownames(germplasmNameBLUPs) <- NULL

grain_g_BLUPs <- germplasmNameBLUPs


##### heading

lm_heading <- lme4::lmer(heading ~ (1|germplasmName) + (1|year_block), data = filter(data,!is.na(heading)))

df_heading <- as.data.frame(VarCorr(lm_heading))
vg_heading <- df_heading[df_heading$grp == "germplasmName","vcov"]
ve_heading <- df_heading[df_heading$grp == "Residual", "vcov"]
h2_heading <- vg_heading/(vg_heading + ve_heading/(2))

allBLUPs <- ranef(lm_heading)
germplasmNameBLUPs <- allBLUPs$germplasmName
colnames(germplasmNameBLUPs)[1] <- "heading_BLUPs"

germplasmNameBLUPs$Genotype <- rownames(germplasmNameBLUPs)
germplasmNameBLUPs <- germplasmNameBLUPs[,c(2,1)]
rownames(germplasmNameBLUPs) <- NULL

heading_BLUPs <- germplasmNameBLUPs





### calculate SI and select top percent  ### used to select the plots to keep in the spring

survival_BLUPs %>% 
  left_join(damage_BLUPs) %>% 
  mutate(EV_survival = 1) %>% 
  mutate(EV_damage = -10) %>% 
  mutate(selection_index = (survival_BLUPs*EV_survival)+ (damage_BLUPs*EV_damage)) %>% 
  slice_max(selection_index,prop = 0.1)


selected_accessions <- survival_BLUPs %>% 
  left_join(damage_BLUPs) %>% 
  mutate(EV_survival = 1) %>% 
  mutate(EV_damage = -10) %>% 
  mutate(selection_index = (survival_BLUPs*EV_survival)+ (damage_BLUPs*EV_damage)) %>% 
  slice_max(selection_index,prop = 0.1) %>% 
  select(Genotype)



CU_2024_2025_SOAP_HR_phenotypes %>%
  
  filter(studyYear != '2024') %>% 
  
  filter(germplasmName %in% c(selected_accessions$Genotype)) %>% 
  select(observationUnitName,germplasmName) %>% 
  mutate(ManagementFactorSeedIncrease = 1) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)

######
lm_data <- survival_BLUPs %>% 
  left_join(damage_BLUPs) %>% 
  left_join(fall_growth_BLUPs) %>%
  left_join(rust_BLUPs) %>%
  left_join(heading_BLUPs) %>% 
  left_join(grain_g_BLUPs) %>%
  filter(!is.na(grain_g_BLUPs))


lm_data

lm_yield_damage <- lm(grain_g_BLUPs ~ (damage_BLUPs), data = lm_data)

lm_yield_damage


lm_yield_survival <- lm(grain_g_BLUPs ~ (survival_BLUPs), data = lm_data)

lm_yield_survival



lm_yield_rust <- lm(grain_g_BLUPs ~ (rust_BLUPs), data = lm_data)

lm_yield_rust

plot(lm_data$rust_BLUPs,lm_data$grain_g_BLUPs)


plot(lm_data$heading_BLUPs,lm_data$grain_g_BLUPs)
lm_yield_heading <- lm(grain_g_BLUPs ~ (heading_BLUPs), data = lm_data)

lm_yield_heading

x= -4.016e+00


####### making selections for fall planting plots  

survival_BLUPs %>% 
  left_join(damage_BLUPs) %>% 
  left_join(fall_growth_BLUPs) %>%
  left_join(rust_BLUPs) %>%
  left_join(heading_BLUPs) %>% 
  left_join(grain_g_BLUPs) %>%
  filter(!is.na(grain_g_BLUPs)) %>% 
  
  mutate(EV_survival = 0.8299) %>% 
  mutate(EV_damage = -11.86) %>% 
  mutate(EV_heading = -4.016) %>%
  mutate(EV_rust = -0.1785714) %>%
  mutate(EV_fall_growth = 0) %>%
  mutate(EV_grain_g = 1) %>%
  
  mutate(h2_survival = h2_heading) %>% 
  mutate(h2_damage = h2_damage) %>% 
  mutate(h2_heading = h2_heading) %>%
  mutate(h2_rust = h2_rust) %>%
  mutate(h2_fall_growth = h2_fall_growth) %>%
  mutate(h2_grain_g = h2_grain_g) %>%
  
  mutate(selection_index = 
           (survival_BLUPs*EV_survival) + 
           (damage_BLUPs*EV_damage) + 
           (heading_BLUPs*EV_heading) + 
           (rust_BLUPs*EV_rust) + 
           (fall_growth_BLUPs*EV_fall_growth) + 
          (grain_g_BLUPs*EV_grain_g)) %>% 
  slice_max(selection_index,prop = 1) %>% 
    left_join((data %>% 
                filter(!is.na(grain_g)) %>% 
                select(germplasmName,grain_g) %>% 
                group_by(germplasmName) %>% 
                summarize(seed_g = sum(grain_g),
                          n=n())), join_by('Genotype' == 'germplasmName')) %>% 

write.table("clipboard", sep="\t", row.names=FALSE)


#####
data %>% 
  filter(!is.na(grain_g)) %>% 
  select(germplasmName,grain_g) %>% 
  group_by(germplasmName) %>% 
  summarize(seed_g = sum(grain_g))




### extra

survival_BLUPs %>% 
  left_join(damage_BLUPs) %>% 
  left_join(fall_growth_BLUPs) %>% 
  mutate(EV_survival = 6) %>% 
  mutate(EV_damage = -4) %>% 
  mutate(EV_fall_growth = 2) %>%  
  mutate(selection_index = (survival_BLUPs*EV_survival)+ (damage_BLUPs*EV_damage)+ (EV_fall_growth * fall_growth_BLUPs)) %>% 
  slice_max(selection_index,prop = 0.05)




df <- survival_BLUPs %>% 
  left_join(damage_BLUPs) %>% 
  left_join(fall_growth_BLUPs)

plot(df$survival_BLUPs,df$damage_BLUPs)

plot(df$survival_BLUPs,df$fall_growth_BLUPs)





vcf <- read.vcfR("data/SOAP_selections.vcf", verbose = FALSE )


head(vcf)

vcf@gt




