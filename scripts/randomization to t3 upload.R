

### Code for taking randomization to T3/oat and field plan


library(tidyverse)
library(readxl)
library(readr)


## the randomization  # replace here

randomization <- winterOatPea_2025_plot_HR <- read_csv("output/winterOatPea_2025_plot_HR.csv")


##  the map # replace here 
map <- CU_2025_Ithaca_WOP_PLOT_layout <- read_excel("data/2025 Oat-Pea Maps 9.24.24.xlsx", 
                                             sheet = "plot_layout")


#### ENTER FIELD META DATA HERE ####

# trail name convention mother-ship_year_location(short)_trial abbreviation_type(PLOT or HR) 

trial_name = "CU_2025_Ithaca_WOP_PLOT"
breeding_program = "Intercropping Cooperative"
location = "Ithaca, NY - Snyder"
year = "2025"
design_type = ""
description =  "2025 winter oat pea intercrop trial located in Ithaca"
trial_type = "phenotyping_trial"
plot_width = "1.26"
plot_length = ""
field_size = ""     #this will be calculated and added in
planting_date =  "2024-10-04"
harvest_date  = ""




###T3 trial headers 


t3_trial_upload_headers <- c("trial_name",	"breeding_program",	"location",	"year",	"design_type",	"description",	"trial_type",	
                             "plot_width", "plot_length",	"field_size",	"planting_date",	"harvest_date",	"plot_name",	"accession_name",
                             "plot_number",
                             "block_number",	"is_a_control",	"rep_number",	"range_number",	"row_number",	"col_number",
                             "seedlot_name",	"num_seed_per_plot",	"weight_gram_seed_per_plot",	"entry_number",	"is_private")



### Field Meta Calculations  ###


#Two methods for calculating total number of plots make sure they match!

length(randomization$plotNo) # how many plots are there?
max(randomization$plotNo) # how many plots are there?


if_else(length(randomization$plotNo) == max(randomization$plotNo), "ALL GOOD" , "DANGER")


total_plots  <- length(randomization$plotNo)

total_plots




# row_number, col_number and field size in hectares

randomization_row_col  <-  map %>% 
  pivot_longer(!row, names_to = "column", values_to = "plot") %>% 
  mutate(row_number = row) %>% 
  mutate(col_number = as.numeric(column)) %>% 
  mutate(plot_number = plot) %>% 
  select(plot_number,	row_number,col_number) %>% 
  filter(!is.na(plot_number)) %>% 
  arrange(plot_number)

row_number <- randomization_row_col$row_number
col_number <- randomization_row_col$col_number


field_size = ((max(randomization_row_col$row_number)) * 5.5) * 
            ((max(randomization_row_col$col_number)) * 1.5 ) / 10000





### Create data frame of trial level meta


trial_level_headers <- c("trial_name",	"breeding_program",	"location",	"year",	"design_type",	"description",	"trial_type",	
                      "plot_width", "plot_length",	"field_size",	"planting_date",	"harvest_date")




trial_meta <- bind_cols(trial_name,  #keep in this order, to match t3 template
          breeding_program,
          location,
          year,
          design_type,
          description,
          trial_type,
          plot_width,
          plot_length,
          field_size,
          planting_date,
          harvest_date)

colnames(trial_meta) <- trial_level_headers

trial_meta <- trial_meta  %>%  slice(rep(1:n(), each = total_plots)) # duplicate the meta to match the number of plots

trial_meta



### plot level data

plot_level_headers <- c("plot_name",	"accession_name", "plot_number",
                        "block_number",	"is_a_control",	"rep_number",	"range_number",	"row_number",
                        "col_number", "seedlot_name",	"num_seed_per_plot",
                        "weight_gram_seed_per_plot",	"entry_number",	"is_private")

randomization


number_of_blocks <- length(unique(randomization$block))


# check if the number of plots in each block are equal
table(randomization$block)

plots_per_block <- unique(table(randomization$block)) # if the number of plots are not equal in all blocks this will mess things up


stopifnot(length(plots_per_block)==1)  # if the number of plots per block are not equal "length(plots_per_block)" will be greater than 1

total_plots
number_of_blocks
plots_per_block


# double check that the plot and block numbers match up
if_else(total_plots == number_of_blocks*plots_per_block, "ALL GOOD" , "DANGER")

stopifnot(total_plots == number_of_blocks*plots_per_block)




### What are the pea accessions?
pea_accessions <- randomization %>% 
  filter(crop == "Pea") %>%
  select(accession) %>% 
  unique()

pea_accessions


### Trial set up  ###


# preliminary calculation
trial <- randomization %>% 
  mutate(plot_number = (rep(seq(1:plots_per_block), times = number_of_blocks)) +   # set the plot numbers from sequential to 3 digit with block and plots per block
           (rep(seq(1:number_of_blocks), each = plots_per_block)*100)) %>% 
  
  mutate(accession_name = if_else(crop == "Pea", "NO_OATS_PLANTED",accession)) %>% # change pea accessions to NO_OATS_PLANTED which is a valid t3 oat accession
  
  mutate(Blaze = if_else(accession == "Blaze", "1",""))   # need to add in for each pea accession.  This will be used to create the pea managment factor.
  



# renaming for T3 upload
trial <- trial %>% 
  
  mutate(plot_name = str_c(trial_name,"_",plot_number)) %>% 
  
  mutate(block_number  = as.numeric(str_remove(randomization$block, "Block" ))) %>% 
  
  mutate(is_a_control = "") %>% 
  
  mutate(rep_number = "") %>% 
  
  mutate(range_number ="") %>% 
  
  mutate(row_number = row_number) %>%
  
  mutate(col_number = col_number) %>% 
  mutate(seedlot_name = "") %>% 
  mutate(num_seed_per_plot = "") %>% 
  mutate(weight_gram_seed_per_plot = "") %>% 	
  mutate(entry_number = "") %>% 
  mutate(is_private = "")
  
  
  
trial <- trial %>% 
  select(plot_name,
          accession_name,
          plot_number,
          block_number,
          is_a_control, 
          rep_number,               
          range_number,
          row_number,     
          col_number,             
          seedlot_name,         
          num_seed_per_plot,      
          weight_gram_seed_per_plot,
          entry_number,
          is_private,
          Blaze)   ### need to add in peas at the end, plots with peas get a 1 others are empty they will be converted to management factor upon upload


full_trial <- bind_cols( trial_meta ,trial) 


write.csv(full_trial,str_c("output/",trial_name,"_t3_upload.csv"), row.names = FALSE)


