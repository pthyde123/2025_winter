

library(readxl)

Cornell_WinterOatFounders_2024_Headrow_phenotypes <- read_excel("data/Cornell_WinterOatFounders_2024_Headrow_phenotypes.xlsx")

WOF_meta <- read_excel("data/WOF_meta.xlsx")


colnames(Cornell_WinterOatFounders_2024_Headrow_phenotypes)

colnames(WOF_meta)


Cornell_WinterOatFounders_2024_Headrow_phenotypes %>%
  select("germplasmName","observationUnitName","Heading - %|CO_350:0005127","Grain weight - g|CO_350:0005123") %>% 
  left_join(WOF_meta %>% 
              select(accession,ORIGIN,NARRATIVE,LODGING,"GROWTH HABIT","DAYS TO ANTHESIS","STRAW BREAKAGE","YIELD","IMPROVEMENT STATUS",PI_GRIN,Site,list,latitude,longitude,),join_by(germplasmName==accession)) %>% 
  print(n=nrow(Cornell_WinterOatFounders_2024_Headrow_phenotypes)) %>% 
  write.csv( "output/WOF_seed.csv", row.names = F)
  

