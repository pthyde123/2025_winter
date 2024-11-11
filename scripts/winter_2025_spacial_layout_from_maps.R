

####  trial coordinates may be uploaded in tab-delimited text file format
####  (.xls or .xlsx formats are NOT supported)


####Required fields:
####  plot_name (must exist in the database for this trial)
####  row_number (needed for the display of physical layout)
####  col_number (needed for the display of physical layout)

####  plot_name	row_number	col_number


##  Notes
##  Field plots will be displayed in the physical trial layout based on the 
###   coordinates you provided in the row and column number.
##  Physical trial layout will capture serpentine, zigzag and any other planting format use.


library(tidyverse)



#### SOAP_2025  ####

trial_name_abrr <- "SOAP_2025"


library(readxl)
SOAP_2025_map <- read_excel("output/SOAP_trial.xlsx", 
                     sheet = "map")

library(readxl)
SOAP_2025_data <-  read_excel("output/SOAP_trial.xlsx", 
                                   sheet = "data")

library(caroline)

SOAP_2025_map %>% 
  pivot_longer(!row, names_to = "column", values_to = "plot") %>% 
  mutate(row_number = row) %>% 
  mutate(col_number = column) %>% 
  mutate(plot_number = plot) %>% 
  select(plot_number,	row_number,col_number) %>% 
  filter(!is.na(plot_number)) %>% 
  arrange(plot_number) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE, col.names=TRUE)

