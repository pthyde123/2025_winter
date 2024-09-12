library(AlgDesign)
library(tidyverse)
library(readxl)

ss <- floor(runif(1, 10000, 1000000))
set.seed(447528) 

# Import selected genotypes
planning <- read_excel("data/planning.xlsx", 
                       sheet = "selections")


# Format dataframe to match JL previous randomization

oatPlanning <- planning %>% 
  select(germplasmName,plots_to_plant,replicates,source) %>% 
  mutate("accession" = germplasmName) %>% 
  mutate("nRepsNY" = replicates) %>% 
  select(source,accession,nRepsNY) 


print(oatPlanning,n=nrow(oatPlanning))


# 8 Blocks
# Each block will have 42 plots
###  18 paired (oat, oat-pea) plots from 2024 plot trial
###  4 unpaired intercrop only plots from 2024 headrow trial
###  1 paired monocrop pea plot


nBlksNY <- 8   #8 blocks
nPlotsPerBlk <- 23   #23 accessions, pairing comes in later 

# make dataframe for AlgDesign::optBlock
plotsNY <- dplyr::tibble(
  accession=rep(oatPlanning$accession, oatPlanning$nRepsNY),  # duplicate accession to the number of replicates needed
  state="NY") %>%
  mutate(crop=dplyr::if_else(grepl("Blaze", accession), "Pea", "Oat"))

  
blkOP <- AlgDesign::optBlock(frml=as.formula("~ accession"),
                             withinData=plotsNY,
                             blocksizes=rep(nPlotsPerBlk, nBlksNY))



getOatSource <- function(accession){
  return(oatPlanning$source[which(oatPlanning$accession == accession)])
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
monoPea <- dplyr::filter, accession=="Blaze")
table(monoPea$block)




# Make it a planting plan



winterOatPea_2025_2 <- winterOatDesign %>% 
  mutate("paired" = if_else(source=="headrow","single","pair")) %>% 
  mutate(pairNo = seq(1:nrow(winterOatDesign))) %>% 
  mutate(pairs = if_else(source=="headrow",1,2)) %>% 
  group_by(pairNo)    ## added in some columns identifying pairs and number of pairs

df <- winterOatPea_2025_2 %>% 
  expand(count = seq(1:pairs)) ## create a dataframe doubling the pairs = 2  

winterOatPea_2025 <- winterOatPea_2025_2 %>% 
  left_join(df) %>% 
  ungroup() ## join the df with the original 

# Randomly assign if the first or second plot in the pair is the monocrop or intercrop
  
winterOatPea_2025 <-  winterOatPea_2025 %>% 
  mutate("SN" = seq(1:nrow(winterOatPea_2025))) %>% 
  group_by(SN) %>% 
  mutate("random" = runif(1)) %>% 
  ungroup() %>% 
  mutate("intercrop" = if_else(count==1,"intercrop","monocrop")) %>% 
  arrange(pairNo,random) %>%
  mutate("plotNo" = seq(1:nrow(winterOatPea_2025))) %>%  ## add the plot numbers 
  select(plotNo,pairNo,block,accession,crop,intercrop,source) %>% 
  mutate(intercrop = if_else(crop=="Pea","monocrop",intercrop)) %>%
  mutate("crop_2" = if_else(crop=="Oat" & intercrop == "monocrop","oat",
                            if_else(crop =="Oat" & intercrop == "monocrop", "oat",
                                      if_else(crop == "Pea", "pea", 
                                              if_else(crop=="Oat" & intercrop == "intercrop","oat-pea","na"))))) ### add oat, pea, oat-pea designation 

# Take a look
print(winterOatPea_2025, n= nrow(winterOatPea_2025)) 


#  Save it
write_csv(winterOatPea_2025, "output/winterOatPea_2025.csv")


  
  
  
  
  
  
