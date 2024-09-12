
### this randomization does the accessions from the plot trial and HR trial 
### separately then combines them. This ensures that there are 4 HR plots in 
### all blocks.



library(AlgDesign)
library(tidyverse)
library(readxl)
library(here)

ss <- floor(runif(1, 10000, 1000000))
set.seed(447528) 

# Import selected genotypes
planning <- read_excel(here::here("data","planning.xlsx"),  
                       sheet = "selections") #added here code


# Format dataframe to match JL previous randomization

oatPlanning <- planning %>% 
  select(germplasmName,plots_to_plant,replicates,source) %>% 
  mutate("accession" = germplasmName) %>% 
  mutate("nRepsNY" = replicates) %>% 
  select(source,accession,nRepsNY) 


print(oatPlanning,n=nrow(oatPlanning))

### removed headrow accessions
### headrow accessions will have there own optBlock so that there are 2 per block

oatPlanning_plots <-  oatPlanning %>% 
  filter(source!="headrow")


# 8 Blocks
# Each block will have 42 plots eventually, in this optBlock there will be 38 plots
###  18 paired (oat, oat-pea) plots from 2024 plot trial
###  1 paired monocrop pea plot

###  4 unpaired intercrop only plots from 2024 headrow trial will have separate optBlock


nBlksNY <- 8   #8 blocks
nPlotsPerBlk <- 19   #19 accessions, pairing comes in later 

# make dataframe for AlgDesign::optBlock
plotsNY <- dplyr::tibble(
  accession=rep(oatPlanning_plots$accession, oatPlanning_plots$nRepsNY),  # duplicate accession to the number of replicates needed
  state="NY") %>%
  mutate(crop=dplyr::if_else(grepl("Blaze", accession), "Pea", "Oat"))




blkOP <- AlgDesign::optBlock(frml=as.formula("~ accession"),
                             withinData=plotsNY,
                             blocksizes=rep(nPlotsPerBlk, nBlksNY))



getOatSource <- function(accession){
  return(oatPlanning_plots$source[which(oatPlanning_plots$accession == accession)])
}



# Assemble the design


winterOatDesign<-tibble(accession=character(), state=character(),crop=character())  # create empty dataframe  

for (blockNum in 1:nBlksNY){
  thisBlock <- as_tibble(blkOP$Blocks[[blockNum]]) %>%
    dplyr::arrange(sample(nrow(blkOP$Blocks[[blockNum]]))) %>%
    dplyr::mutate(block=paste0("Block", blockNum)) %>%
    dplyr::mutate(source=sapply(accession, getOatSource)) %>%
    dplyr::relocate(block, .before=accession)
  winterOatDesign <- dplyr::bind_rows(winterOatDesign, thisBlock)
}


# Some quick checks
monoPea <- dplyr::filter(winterOatDesign, accession=="Blaze")
table(monoPea$block)

plot <- dplyr::filter(winterOatDesign, source=="plot")

table(plot$block)

headrow <- dplyr::filter(winterOatDesign, source=="headrow")

table(headrow$block)



### Create a separate randomization for single plots coming from headrows ###


oatPlanning_HR <-  oatPlanning %>% 
  filter(source=="headrow")

nBlksNY <- 8   #8 blocks
nPlotsPerBlk_HR <- 4   # 4 headrow (HR) accessions per plot 

# make dataframe for AlgDesign::optBlock
plotsHR <- dplyr::tibble(
  accession=rep(oatPlanning_HR$accession, oatPlanning_HR$nRepsNY),  # duplicate accession to the number of replicates needed
  state="NY") %>%
  mutate(crop=dplyr::if_else(grepl("Blaze", accession), "Pea", "Oat"))




blkOP_HR <- AlgDesign::optBlock(frml=as.formula("~ accession"),
                             withinData=plotsHR,
                             blocksizes=rep(nPlotsPerBlk_HR, nBlksNY))

getOatSource_HR <- function(accession){
  return(oatPlanning_HR$source[which(oatPlanning_HR$accession == accession)])
}

# Assemble the design

winterOatDesign_HR<-tibble(accession=character(), state=character(),crop=character())  # create empty dataframe  

for (blockNum in 1:nBlksNY){
  thisBlock <- as_tibble(blkOP_HR$Blocks[[blockNum]]) %>%
    dplyr::arrange(sample(nrow(blkOP_HR$Blocks[[blockNum]]))) %>%
    dplyr::mutate(block=paste0("Block", blockNum)) %>%
    dplyr::mutate(source=sapply(accession, getOatSource_HR)) %>%
    dplyr::relocate(block, .before=accession)
  winterOatDesign_HR <- dplyr::bind_rows(winterOatDesign_HR, thisBlock)
}


winterOatDesign_HR

# Some quick checks
monoPea <- dplyr::filter(winterOatDesign_HR, accession=="Blaze")
table(monoPea$block)

headrow <- dplyr::filter(winterOatDesign_HR, source=="headrow")

table(headrow$block)

plot <- dplyr::filter(winterOatDesign_HR, source=="plot")

table(plot$block)




### Combine plot and headrow design and randomize accessions within block ###


winterOatDesign_combined  <- winterOatDesign %>% 
  bind_rows(winterOatDesign_HR)

  

winterOatDesign_combined  <-  winterOatDesign_combined %>% 
  mutate("random" = runif(1:nrow(winterOatDesign_combined))) %>% 
  arrange(block,random) %>% 
  select(!random)


# Some quick checks
monoPea <- dplyr::filter(winterOatDesign_combined, accession=="Blaze")
table(monoPea$block)

headrow <- dplyr::filter(winterOatDesign_combined, source=="headrow")

table(headrow$block)

plot <- dplyr::filter(winterOatDesign_combined, source=="plot")

table(plot$block)





### Make it a planting plan  ###



winterOatPea_2025_2 <- winterOatDesign_combined %>% 
  mutate("paired" = if_else(source=="headrow","single","pair")) %>% 
  mutate(pairNo = seq(1:nrow(winterOatDesign_combined))) %>% 
  mutate(pairs = if_else(source=="headrow",1,2)) %>% ## actually numeric is more useful
  group_by(pairNo)    ## added in some columns identifying pairs and number of pairs

df <- winterOatPea_2025_2 %>% 
  expand(count = seq(1:pairs)) ## create a dataframe doubling the pairs = 2  

winterOatPea_2025_3 <- winterOatPea_2025_2 %>% 
  left_join(df) %>% 
  ungroup() ## join the df with the original 

# Randomly assign if the first or second plot in the pair is the monocrop or intercrop

winterOatPea_2025 <-  winterOatPea_2025_3 %>% 
  mutate("SN" = seq(1:nrow(winterOatPea_2025_3))) %>% 
  group_by(SN) %>% 
  mutate("random" = runif(1)) %>% 
  ungroup() %>% 
  mutate("intercrop" = if_else(count==1,"intercrop","monocrop")) %>% 
  arrange(pairNo,random) %>%
  mutate("plotNo" = seq(1:nrow(winterOatPea_2025_3))) %>%  ## add the plot numbers 
  select(plotNo,pairNo,block,accession,crop,intercrop,source) %>% 
  mutate(intercrop = if_else(crop=="Pea","monocrop",intercrop)) %>%
  mutate("crop_2" = if_else(crop=="Oat" & intercrop == "monocrop","oat",
                            if_else(crop =="Oat" & intercrop == "monocrop", "oat",
                                    if_else(crop == "Pea", "pea", 
                                            if_else(crop=="Oat" & intercrop == "intercrop","oat-pea","na"))))) ### add oat, pea, oat-pea designation 

# Take a look
print(winterOatPea_2025, n= nrow(winterOatPea_2025)) 


# Some quick checks
monoPea <- dplyr::filter(winterOatPea_2025, accession=="Blaze")
table(monoPea$block)

headrow <- dplyr::filter(winterOatPea_2025, source=="headrow")

table(headrow$block)

plot <- dplyr::filter(winterOatPea_2025, source=="plot")

table(plot$block)





#  Save it
write.csv(winterOatPea_2025, here::here("output","winterOatPea_2025_plot_HR.csv")) #added here code








