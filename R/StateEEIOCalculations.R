# StateEEIOCalculations.R


## Primary State CBE function. 
## Returns by default a vector with GHG in CO2e totals by sector (rows)
calculateStateCBE <- function(model, CO2e=TRUE, perspective="FINAL",
                              domestic=FALSE, RoUS=FALSE) {
  loc <- getLocation(RoUS, model)
  r <- useeior::calculateEEIOModel(model,
                                   perspective = perspective,
                                   demand = "Consumption",
                                   location = loc,
                                   use_domestic_requirements = domestic,
                                   household_emissions = TRUE)
  # Note this function requires a model with only a single indicator
  if(CO2e) {
    r<-r$LCIA_f
  } else {
    r<-r$LCI_f
  }
  return(r)
}

## Returns a vector of demand in dollars by type with sectors as rows
#' @param type, str, "Household", "Federal Government", "State Government", "Investment", "final", or "intermediate"
getStateUsebyType <- function(model, type="final", domestic=FALSE, RoUS=FALSE) {
  opt <- c("Household", "Federal Government", "State Government", "Investment", "final", "intermediate")
  if (!type %in% opt) {
    stop(paste0("'type' options are ", paste(opt, collapse=", ")))
  }
  loc <- getLocation(RoUS, model)
  if (type=="final") {
    code_loc <- model$FinalDemandMeta[endsWith(model$FinalDemandMeta$Code_Loc,loc),][["Code_Loc"]]
  } else if (type=="intermediate") {
    code_loc <- model$Commodities$Code_Loc[endsWith(model$Commodities$Code_Loc,loc)]
  } else {
    code_loc <- model$FinalDemandMeta[model$FinalDemandMeta$Group == type &
                                      endsWith(model$FinalDemandMeta$Code_Loc,loc),][["Code_Loc"]]
  }
  ## TODO: FURTHER HANDLE STATE VS FEDERAL GOVT
  if (domestic) {
    U <- model$U_d
  } else {
    U <- model$U
  }
  name <- type
  # Sum across demand columns, drop the Value Add rows
  usebytype <- as.matrix(rowSums(U[-(which(startsWith(rownames(U), "V00"))), code_loc, drop=FALSE]))
  colnames(usebytype) <- name
  
  return(usebytype)
}


# Calculate demand by sector by type
calculateDemandByType <- function(model, price_year=NULL) {
  demand_by_type <- data.frame(sapply(c("Household", "Investment", "Federal Government", "State Government"),
                                        getStateUsebyType, model=model,
                                        simplify=FALSE, USE.NAMES=FALSE))
  demand_by_type <- cbind(demand_by_type, Total = rowSums(demand_by_type))
  
  # if desired, adjust price type before summing
  if(is.null(price_year)) {
    price_year <- model$specs$IOYear
  }
  rho <- model$Rho[, toString(model$specs$IOYear)] / model$Rho[, toString(price_year)]
  demand_by_type <- demand_by_type * rho
  
  total_demand_by_type <- as.matrix(colSums(demand_by_type))
  colnames(total_demand_by_type) <- "Demand"
  return(total_demand_by_type)
}

getDemandbyRegion <- function(model, region="SoI") {
  state <- model$specs$ModelRegionAcronyms[[1]]
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
  if(region=="SoI") {
    d <- soi_soi_finalconsumption
  } else if (region=="RoUS") {
    d <- soi_rous_finalconsumption
  } else if (region=="ROW") {
    d <- soi_import_consumption
  }
  d <- as.matrix(d)
  return(d)
}

# Calculate demand by sector by region
calculateDemandByRegion <- function(model, price_year=NULL) {
  demand_by_region <- data.frame(sapply(c("SoI", "RoUS", "ROW"),
                                        getDemandbyRegion, model=model,
                                        simplify=FALSE, USE.NAMES=TRUE))
  demand_by_region <- cbind(demand_by_region, Total = rowSums(demand_by_region))
  
  # if desired, adjust price type before summing
  if(is.null(price_year)) {
    price_year <- model$specs$IOYear
  }
  rho <- model$Rho[, toString(model$specs$IOYear)] / model$Rho[, toString(price_year)]
  demand_by_region <- demand_by_region * rho
  
  total_demand_by_region <- as.matrix(colSums(demand_by_region))
  colnames(total_demand_by_region) <- "Demand"
  return(total_demand_by_region)
}


reformatStatebyYearLongtoWide <- function(df, value.var) {
  colnames(df) <- c(value.var, "State", "Year")
  df_wide <- reshape(df,
                     v.names = value.var,
                     idvar = "State",
                     timevar = "Year",
                     direction = "wide")
  row.names(df_wide) <- df_wide$State # Make row names the states
  df_wide <- df_wide[,-1] # Remove the column with state names
  colnames(df_wide) <- years
  df_wide <- df_wide[order(rownames(df_wide)), order(colnames(df_wide))]
  return(df_wide)
}

reformatWidetoLong <- function(df) {
  df <- melt(df, varnames=c('variable', 'ID'))
  x <- do.call('rbind', (strsplit(as.character(df$ID), "-", fixed=TRUE)))
  if(ncol(x) == 2) {
    colnames(x) <- c("State", "Year")
  } else if (ncol(x) == 1) {
    colnames(x) <- c("Year")
  } else {
    stop("Error in reformatting")
  }
  df <- cbind(x, df)
  return(df)
}


convertStateResultFormatToStatebyYear <- function(df, value.var) {
  df_names <- t(data.frame(strsplit(row.names(df),'-')))
  df <- cbind(df,df_names)
  df <- reformatStatebyYearLongtoWide(df, value.var=value.var)
  return(df)
}


getLocation <- function(RoUS, model) {
  if (RoUS) {
    loc <- "RoUS"
  } else {
    loc <- model$specs$ModelRegionAcronyms[1]
  }
  return(loc)
}

aggregateStateResultMatrix <- function(model, matrix, RoUS=FALSE) {
  name <- colnames(matrix)
  if (RoUS) {
    matrix <- subset(matrix, endsWith(rownames(matrix), "RoUS"))  
  } else {
    matrix <- subset(matrix, !(endsWith(rownames(matrix), "RoUS")))
  }
  matrix <- useeior:::aggregateResultMatrixbyRow(matrix, "Sector", model$crosswalk)
  colnames(matrix) <- name
  # reorder matrix rows
  rows <- subset(unique(model$crosswalk$BEA_Sector), unique(model$crosswalk$BEA_Sector) %in% rownames(matrix))
  matrix <- matrix[rows,,drop=FALSE]
  
  return(matrix)
}


subsetColumnsByString <- function(matrix, s) {
  m <- matrix[, stringr::str_detect(colnames(matrix), s)]
  return(m)
}

# Returns the territorial inventory in Result format
# constructed from the model's Total by Sector amounts and indicator GWPs
getStateGHGI <- function(model) {
  loc <- model$specs$ModelRegionAcronyms[1]
  fields <- c("Sector","Flowable","FlowAmount",   "Location")
  GHGI <- useeior:::collapseTBS(model$TbS, model)[,fields] 
  # filter out other regions (RoUS)
  GHGI <- GHGI[GHGI$Location==loc,]
  GWPs <- model$Indicators$factors[,c("Flowable","Amount")]
  GWPs <- unique(GWPs)
  GHGI <- merge(GHGI, GWPs, all.x=TRUE,)
  ## ^^ TODO not capturing kg CO2e flows like HFCs and PFCs unspecified
  
  GHGI$`Greenhouse Gases` <- GHGI$FlowAmount*GHGI$Amount
  GHGI <- aggregate(`Greenhouse Gases` ~ Sector, GHGI, sum)
  # Merge in sectors in case some are missing
  comms_in_m <- list(Sector=unique(model$Commodities$Code))
  GHGI <- merge(GHGI, comms_in_m, all=TRUE)   
  row.names(GHGI) <-  apply(cbind(GHGI['Sector'], loc), 1, FUN = joinStringswithSlashes)
  GHGI <- matrix(GHGI[,c("Greenhouse Gases")],
                 dimnames=list(rownames(GHGI), c("Greenhouse Gases")))
  ## TODO update order of sectors before returning
  
  return(GHGI)  
}

# Combine two or more results vectors passed in a named vector; sets the ID equal
# to the name used in the named vector
# Returns a dataframe
combineResults <- function(dfNames) {
  df <- do.call(rbind, lapply(dfNames, function(x) {
    data.frame(ID=x, Sector=rownames(get(x)), get(x))
  }))
  df <- setNames(df, c("ID", "Sector", "Value"))
  y <- setNames(names(dfNames), dfNames)
  df$ID <- stringr::str_replace_all(df$ID, y)
  rownames(df) <- NULL
  
  return (df)
}

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

