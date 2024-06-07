## Validate that StateCBEResults aggregation and split into SoI,RoUS,RoW
## adds back up StateCBEResult for total

source("../R/StateEEIOFigures.R")
source("../R/StateEEIOCalculations.R")
library(useeior)
library(reshape2)

#Set model name
modelname <- "MEEEIOv1.1-GHGc-19"
file_path <- file.path("..","models",paste0(modelname,".rds")) #download_model_RDS(modelname)
model <- readRDS(file_path) # Test that it loads correctly

ghg_total <- calculateStateCBE(model)
ghg_domestic <- calculateStateCBE(model, domestic=TRUE)
ghg_RoW <- ghg_total - ghg_domestic

ghg_agg_soi <- aggregateStateResultMatrix(model, ghg_domestic, RoUS=FALSE)
ghg_agg_rous <- aggregateStateResultMatrix(model, ghg_domestic, RoUS=TRUE)
ghg_agg_row <- aggregateStateResultMatrix(model, ghg_RoW, RoUS=FALSE) + aggregateStateResultMatrix(model, ghg_RoW, RoUS=TRUE)


ghg_agg <- cbind(ghg_agg_soi,ghg_agg_rous,ghg_agg_row)

reldiff <- (sum(ghg_agg)-sum(ghg_total))/sum(ghg_total)
print(reldiff)