# StateEEIOCalculations.R


#' Prepare a dataframe for graphing from list of two-region models
#' @param model_list List of completed EEIO models
#' @param matrix_name Name of model matrix to extract data from, e.g. "B", set to NULL to use result vector
#' @param perspective Result perspective "DIRECT" or "FINAL", only used if matrix_name = NULL
#' @param indicator Row name in the specified matrix for the figure
#' @param sector_to_remove Code of one or more BEA sectors that will be removed from the plot. Can be "".
#' @param domestic, bool, TRUE to use domestic results.
#' @param demand, str, "Production" or "Consumption"
#' @param combine_SoIRoUS, TRUE to aggregate results from RoUS and SOI
#' @param household_emissions, pass through to calculateEEIOModel
prepareDFforFigure <- function(model_list, matrix_name=NULL, perspective=NULL,
                               indicator, sector_to_remove="", domestic=FALSE, 
                               demand="Production", combine_SoIRoUS=FALSE, household_emissions=FALSE) {
  # Prepare data frame for plot
  df <- data.frame()
  for (modelname in names(model_list)) {
    model <- model_list[[modelname]]
    # Generate BEA sector color mapping
    mapping <- useeior:::getBEASectorColorMapping(model)
    mapping$SummaryCode <- toupper(mapping$SummaryCode)
    mapping$GroupName <- mapping$SectorName
    # Generate matrix or result
    if (is.null(matrix_name)){
      result <- useeior::calculateEEIOModel(model, perspective = perspective,
                                            demand = demand, location=modelname,
                                            use_domestic_requirements = domestic
                                            # household_emissions = household_emissions
      )
      matrix <- t(result[[2]])
      # Extract demand value
      demand_name <- ifelse(domestic,
                            paste0("Domestic", demand),
                            paste0("Complete", demand))
      # Get vector name (ID) from the meta table
      id <- model$DemandVectors$meta[which(model$DemandVectors$meta$Name==demand_name &
                                             model$DemandVectors$meta$Location==modelname),"ID"]
      d <- model$DemandVectors$vectors[[id]]
    } else {
      d <- NULL
      matrix <- model[[matrix_name]]
    }
    if(!is.null(indicator)){
      matrix <- matrix[indicator, , drop = FALSE]      
    }
    
    matrix <- as.data.frame(reshape2::melt(matrix))
    colnames(matrix) <- c("Indicator", "Sector", "Value")
    matrix$modelname <- modelname
    matrix$region <- sapply(strsplit(as.character(matrix$Sector),"/"), "[", 2)
    if(!is.null(d)) {
      matrix <- merge(matrix, data.frame(demand=d), by.x = "Sector", by.y = 0, all.x=TRUE)
    }
    matrix$Sector <- toupper(gsub("/.*", "", matrix$Sector))
    # Convert matrix to df
    df_model <- data.frame()
    df_model <- rbind(matrix, df_model)
    df_model <- merge(df_model, mapping[, c(paste0(model$specs$BaseIOLevel, "Code"), "color", "GroupName")],
                      by.x = "Sector", by.y = paste0(model$specs$BaseIOLevel, "Code"))
    if (model$specs$CommodityorIndustryType=="Commodity") {
      SectorName <- model$Commodities[, c("Code", "Name")]
    } else {
      SectorName <- model$Industries[, c("Code", "Name")]
    }
    if(household_emissions)
      SectorName <- rbind(SectorName, data.frame(Code = c("F010", "F01000"),
                                                 Name = c("Households", "Households")))
    # Remove duplicate sector names for two-region models
    SectorName <- SectorName[!duplicated(SectorName), ]
    SectorName$Code <- toupper(SectorName$Code)
    colnames(SectorName) <- c("Sector", "SectorName")
    df_model <- merge(df_model, SectorName, by = "Sector")
    # Remove certain sectors
    df_model <- df_model[!df_model$Sector%in%sector_to_remove, ]
    df_model <- df_model[order(df_model$GroupName), ]
    df <- rbind(df, df_model)
  }
  
  if (combine_SoIRoUS) {
    # for results, sum SoI and RoUS
    df_agg <- dplyr::group_by(df, Indicator, Sector, color, modelname,
                              GroupName, SectorName)
    df_agg <- dplyr::summarize(
      df_agg,
      ValueAgg = sum(Value),
      .groups = 'drop'
    )
    colnames(df_agg)[colnames(df_agg)=="ValueAgg"] <- "Value"
    df <- df_agg
  }
  return(df)
}


calculateDemandVectors <- function(model, price_year=NULL) {
  
  state <- model$specs$ModelRegionAcronyms[[1]]
  year <- model$specs$IOYear
  if(is.null(price_year)) {
    price_year <- toString(year)
  }
  
  # Get consumption amounts by region
  # The 3rd demand vector is Consumption complete
  # The 4th is Consumption domestic. 
  # The difference should represent total consumption by each
  
  soi_comms <- grep(state, names(model$DemandVectors$vectors[[4]]))
  rous_comms <- grep("RoUS", names(model$DemandVectors$vectors[[4]]))
  
  soi_soi_finalconsumption <- model$DemandVectors$vectors[[4]][soi_comms]
  soi_rous_finalconsumption <- model$DemandVectors$vectors[[4]][rous_comms]
  
  soi_import_consumption <- model$DemandVectors$vectors[[3]]-model$DemandVectors$vectors[[4]]
  
  soi_import_consumption <- soi_import_consumption[soi_comms]
  
  # Adjust price years using Rho
  rho_full <- model$Rho[, toString(year)] / model$Rho[, price_year]
  rho <- rho_full[soi_comms]
  soi_consumption <- cbind("In State" = soi_soi_finalconsumption * rho,
                           "Rest of US" = soi_rous_finalconsumption * rho,
                           "Imported" = soi_import_consumption * rho)
  
  total_demand <- colSums(soi_consumption)
  total_demand <- data.frame(t(total_demand))
  total_demand <- cbind(total_demand, Total = rowSums(total_demand))
  total_demand$Year <- toString(year)
  
  # Calculate demand by sector by source
  # drop value added rows
  use <- head(model$U, -6)
  households <- model$FinalDemandMeta[model$FinalDemandMeta$Group == "Household" &
                                    endsWith(model$FinalDemandMeta$Code_Loc, state), ][["Code_Loc"]]
  investment <- model$FinalDemandMeta[model$FinalDemandMeta$Group == "Investment" &
                                    endsWith(model$FinalDemandMeta$Code_Loc, state), ][["Code_Loc"]]
  state_govt <- model$FinalDemandMeta[(model$FinalDemandMeta$Group == "Government") &
                                    endsWith(model$FinalDemandMeta$Code_Loc, state) &
                                    startsWith(model$FinalDemandMeta$Code, "F10"), ][["Code_Loc"]]
  fed_govt <- model$FinalDemandMeta[(model$FinalDemandMeta$Group == "Government") &
                                  endsWith(model$FinalDemandMeta$Code_Loc, state)&
                                  !startsWith(model$FinalDemandMeta$Code, "F10"), ][["Code_Loc"]]
  demand_by_source <- cbind("Households" = use[, households] * rho_full,
                            "Investment" = rowSums(use[, investment]) * rho_full,
                            "State and Local Government" = rowSums(use[, state_govt]) * rho_full,
                            "Federal Government" = rowSums(use[, fed_govt]) * rho_full
  )
  demand_by_source <- cbind(demand_by_source, Total = rowSums(demand_by_source))
  total_demand_by_source <- colSums(demand_by_source)
  total_demand_by_source <- data.frame(t(total_demand_by_source))
  total_demand_by_source$Year <- toString(year)
  
  return(list("demand" = total_demand, "demand_by_source" = total_demand_by_source))
}
  