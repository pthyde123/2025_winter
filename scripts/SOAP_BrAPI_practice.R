




library(devtools)
install_github("TriticeaeToolbox/BrAPI.R")
library(BrAPI)
library(tidyverse)



# Manually set the BrAPI Server host
oat <- createBrAPIConnection("oat.triticeaetoolbox.org")


r = oat$get("observations", query=list(studyDbId=6800))



selected_breeding_program <- "Intercropping Cooperative"
resp <- oat$get("/studies", query=list(programName=selected_breeding_program), page="all")
trials <- lapply(resp$combined_data, \(x) { list(id=x$studyDbId, name=x$studyName) })


selected_trials <- c("CU_2025_Ithaca_SOAP_HR")

for ( trial_name in selected_trials ) {
  resp <- oat$get("/studies", query=list(studyName=trial_name))
  trial_metadata <- resp$data[[1]]
  trial_id <- trial_metadata$studyDbId
  location <- trial_metadata$locationName
  planting_date <- trial_metadata$startDate
  harvest_date <- trial_metadata$endDate
  design <- trial_metadata$experimentalDesign$PUI
}




selected_trial_id <- "6800"
resp <- oat$get("/observationunits", query=list(studyDbId=selected_trial_id), page="all", pageSize=100)
for ( plot in resp$combined_data ) {
  plot_id <- plot$observationUnitDbId
  plot_name <- plot$observationUnitName
  row <- plot$observationUnitPosition$positionCoordinateY
  col <- plot$observationUnitPosition$positionCoordinateX
  accession <- plot$germplasmName
}




################

library(BrAPI)
library(tidyverse)

# Manually set the BrAPI Server host
oat <- createBrAPIConnection("oat.triticeaetoolbox.org")


# Get all of the observations for a single trial
selected_trial_id <- "6800"
resp <- oat$get("/observations", query=list(studyDbId=selected_trial_id), page="all", pageSize=500)
observations <- resp$combined_data

# Get the unique set of trait names observed in this trial
trait_names <- sort(unique(sapply(observations, \(x) { x$observationVariableName } )))

# Get the unique set of accession names in this trial
accession_names <- sort(unique(sapply(observations, \(x) { x$germplasmName } )))

# Build a long-format table of trait observations
data <- tibble(
  plot_id = numeric(),
  plot_name = character(),
  blockNumber = numeric(),
  accession_name = character(),
  trait_name = character(),
  value = numeric()
)

for ( observation in observations ) {
  data <- rbind(data, tibble(
    plot_id = as.numeric(observation$observationUnitDbId),
    plot_name = observation$observationUnitName,
    block = observation$blockNumber,
    accession_name = observation$germplasmName,
    trait_name = observation$observationVariableName,
    value = as.numeric(observation$value)
  ))
}


data %>% 
  pivot_wider(names_from = trait_name, values_from = value) %>% 
  mutate(plotNumber = as.numeric(str_sub(plot_name, -4) )) %>% 
  mutate(blockNumber = if_else(plotNumber <2000,1,2))



##########

search <- oat$post('/search/observationunits', body=list(studyDbIds="6800")$content$result$searchResultsDbId)

plots = oat$get(paste0('/search/observationunits/', search))




