library(tidyverse)
library(readr)


winterOatPea_2025_plot_HR <- read_csv("output/winterOatPea_2025_plot_HR.csv")


### making csv for use on Sorrels envelop printer
### file must must have these headers in this order 
###     source,plotNo,oatName,peaName
### arrange by oatname so all plots for a given accession are together for seed separating from bulk bag



winterOatPea_2025_plot_HR %>% 
  mutate(peaName = if_else(crop_2=="oat-pea","Blaze","na")) %>% 
  mutate(oatName = accession) %>% 
  mutate(PlotNo = plotNo) %>% 
  arrange(source,oatName) %>% 
  select(source,plotNo,oatName,peaName) %>% 
  write.csv( "output/winter_2025_envelopes.csv", row.names = F)

