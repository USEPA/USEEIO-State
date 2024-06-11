# StateEEIOCalculations.R
library(reshape2)

## Primary State CBE function. 
## Returns by default a vector with GHG in CO2e totals by sector (rows)
calculateStateCBE <- function(model, CO2e=TRUE, perspective="FINAL",
                              demand="Consumption",domestic=FALSE, RoUS=FALSE) {
  loc <- getLocation(RoUS, model)
  r <- useeior::calculateEEIOModel(model,
                                   perspective = perspective,
                                   demand = demand,
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
  opt <- c("Household", "Federal Government", "State Government", "Government", "Investment", "Export","Import","ChangeInventories", "final", "intermediate")
  if (!type %in% opt) {
    stop(paste0("'type' options are ", paste(opt, collapse=", ")))
  }
  loc <- getLocation(RoUS, model)
  if (type=="final") {
    code_loc <- model$FinalDemandMeta[endsWith(model$FinalDemandMeta$Code_Loc,loc),][["Code_Loc"]]
  } else if (type=="intermediate") {
    code_loc <- model$Industries$Code_Loc[endsWith(model$Industries$Code_Loc,loc)]
  } else if (type=="State Government") {
    code_loc <- model$FinalDemandMeta[model$FinalDemandMeta$Group == "Government" &
                                        endsWith(model$FinalDemandMeta$Code_Loc,loc) &
                                        startsWith(model$FinalDemandMeta$Code, "F10"),][["Code_Loc"]]
  } else if (type=="Federal Government") {
    code_loc <- model$FinalDemandMeta[model$FinalDemandMeta$Group == "Government" &
                                        endsWith(model$FinalDemandMeta$Code_Loc,loc) &
                                        !startsWith(model$FinalDemandMeta$Code, "F10"),][["Code_Loc"]]
  } else {
    code_loc <- model$FinalDemandMeta[model$FinalDemandMeta$Group == type &
                                      endsWith(model$FinalDemandMeta$Code_Loc,loc),][["Code_Loc"]]
  }
  if (domestic) {
    U <- model$U_d
  } else {
    U <- model$U
  }
  name <- type
  # Sum across demand columns, drop the Value Add rows
  usebytype <- as.matrix(rowSums(U[-which(startsWith(rownames(U), "V00")), code_loc, drop=FALSE]))
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
  GWPs <- data.frame("Flowable" = row.names(t(model$C)), t(model$C))
  GWPs$Flowable <- gsub("/.*", "", GWPs$Flowable)
  colnames(GWPs) <- c("Flowable", "Amount")
  GHGI <- merge(GHGI, GWPs, all.x=TRUE,)

  GHGI$`Greenhouse Gases` <- GHGI$FlowAmount*GHGI$Amount
  GHGI <- aggregate(`Greenhouse Gases` ~ Sector, GHGI, sum)
  # Merge in sectors in case some are missing
  comms_in_m <- list(Sector=unique(model$Commodities$Code))
  GHGI <- merge(GHGI, comms_in_m, all=TRUE)   
  row.names(GHGI) <-  apply(cbind(GHGI['Sector'], loc), 1, FUN = useeior:::joinStringswithSlashes)
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

