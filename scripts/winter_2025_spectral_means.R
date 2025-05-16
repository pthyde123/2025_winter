library("dplyr")                                                 
library("plyr")                                                  
library("readr")   
library("readxl") 


### generate QGIS plugin plot boundary rows and columns for META data file  
### 900's are guard rows, 1000's are seed increase

library(tidyverse)
library(readxl)


QGIS_plot_boundary_map <- QGIS_2025_winter_plot_boundary_map <- read_excel("data/QGIS_2025_winter_plot_boundary_map.xlsx")

### this map has the t3 plot numbers with the row column designations assigned using plotBoundary in QGIS
### guard / fill plots are plot number in the 900'S

QGIS_plot_boundary_map


QGIS_plot_boundary_map %>% 
  pivot_longer(!row, names_to = "column", values_to = "plot") %>% 
  mutate(prow = row) %>% 
  mutate(pcol = column) %>% 
  mutate(plot_number = plot) %>% 
  select(plot_number,	prow,pcol) %>% 
  filter(!is.na(plot_number)) %>% 
  slice(rep(1:n(), each = 3)) %>% 
  mutate(grow = 1) %>% 
  mutate(gcol = rep(1:3,times=378)) %>% 
  mutate(subplot = rep(3:1,times=378)) %>% 

write.csv("output/plot_number_to_plot_boundary.csv",row.names=FALSE)






### Import spectral data 04-4-25 ####
spectral_data <- list.files(path = "data/04-4-25_subplot_spectral",     
                            pattern = "*.xlsx", 
                            full.names = TRUE) %>%  
  lapply(read_excel) %>%                                            
  bind_cols() %>% 
  select(1:4,contains("mean")) %>% 
  mutate(prow = prow...1) %>%   ### rename stoped working switched to mutate
  mutate(pcol = pcol...2) %>%
  mutate(grow = grow...3) %>%
  mutate(gcol = gcol...4) %>% 
  mutate(pg_id = str_c(prow,"_",pcol,"_",grow,"_",gcol)) 


spectral_data 

### boundary, grid, plot meta 
plot_number_to_plot_boundary <- read_csv("output/plot_number_to_plot_boundary.csv") 

plot_number_to_plot_boundary <- plot_number_to_plot_boundary %>% 
  mutate(plot_subplot = str_c(plot_number,"_",subplot)) %>% 
  mutate(pg_id = str_c(prow,"_",pcol,"_",grow,"_",gcol))

plot_number_to_plot_boundary


###

df <- spectral_data %>% 
  left_join(plot_number_to_plot_boundary,join_by(pg_id) ) %>% 
  select(plot_number,subplot,plot_subplot,contains("mean")) %>% 
  filter(plot_number < 900) %>% 
  arrange(plot_number,subplot)


CU_2025_Ithaca_WOP_PLOT_phenotypes <- read_csv("data/CU_2025_Ithaca_WOP_PLOT_phenotypes.csv")

spectral_T3 <- CU_2025_Ithaca_WOP_PLOT_phenotypes %>% 
  
  mutate(plot_name = gsub("\\_subplot_.*", "", observationUnitName)) %>% 
  
  filter(observationLevel == "subplot") %>% 
  mutate(subplot_number = str_sub(observationUnitName,-1)) %>% ## DANGER this only works if you filter to subplot first
  
  mutate(plot_subplot = str_c(plotNumber,"_",subplot_number)) %>% 
  
  left_join(df,join_by(plot_subplot)) %>% 
  
  select(observationUnitName,observationUnitDbId,plot_number,subplot,plot_subplot,contains("mean"))


write.csv(spectral_T3,"output/spectral_4-04-25_data.csv",row.names=FALSE)

