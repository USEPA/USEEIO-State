# StateEEIOCalculations.R
library(reshape2)
library(stringr)
#library(useeior) #will require once version is set

## Primary State CBE function. 
## Returns by default a vector with GHG in CO2e totals by sector (rows)
calculateStateCBE <- function(model, CO2e=TRUE, perspective="FINAL",
                              demand="Consumption",domestic=FALSE, RoUS=FALSE,
                              household_emissions=TRUE, show_RoW=TRUE) {
  loc <- getLocation(RoUS, model)
  r <- useeior::calculateEEIOModel(model,
                                   perspective = perspective,
                                   demand = demand,
                                   location = loc,
                                   use_domestic_requirements = domestic,
                                   household_emissions = household_emissions,
                                   show_RoW=show_RoW)
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

# Adjusts a matrix of dollar values in a given IO year to the target price year
adjustDollarMatrixPriceYear <- function (model,matrix,io_year,price_year) {
  rho <- model$Rho[, toString(io_year)] / model$Rho[, toString(price_year)]
  matrix <- t(t(matrix) %*% diag(rho))
  return(matrix)
}


# Calculate demand by sector by type
calculateDemandByType <- function(model, price_year, RoUS=FALSE) {
  demand_by_type <- data.frame(sapply(c("Household", "Investment", "Federal Government", "State Government"),
                                        getStateUsebyType, model=model,RoUS = RoUS,
                                        simplify=FALSE, USE.NAMES=FALSE))
  demand_by_type <- cbind(demand_by_type, Total = rowSums(demand_by_type))
  
  demand_by_type <- adjustDollarMatrixPriceYear(model, demand_by_type, io_year=model$specs$IOYear,
                                                price_year=price_year)
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

aggregateStateResultMatrix <- function(model, matrix, region) {
  name <- colnames(matrix)
  matrix <- subset(matrix, endsWith(rownames(matrix), region))  
  matrix <- useeior:::aggregateResultMatrixbyRow(matrix, "Sector", model$crosswalk)
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
getStateGHGI <- function(model,RoUS=FALSE) {
  loc <- getLocation(RoUS, model)
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

#Calculate CBE in exports to RoUS, exports to RoW, imports from RoUS, imports from ROW
#Add trade balance as exports - imports
calculateCBETradeBalance <- function(model) {
  ##Exports
  # Get exports to RoW
  export_RoW <- getStateUsebyType(model,type="Export")
  # Get exports to RoUS
  RoUS_uses_intermediate <-  getStateUsebyType(model,type="intermediate", domestic=TRUE, RoUS=TRUE)
  RoUS_uses_final <-  getStateUsebyType(model,type="final", domestic=TRUE, RoUS=TRUE)
  RoUS_uses <- RoUS_uses_intermediate+RoUS_uses_final
  #Set uses by RoUS of RoUS commodities to 0 to not count them
  RoUS_uses[grep("RoUS",rownames(RoUS_uses)),] <- 0
  export_RoUS <- RoUS_uses
  ##Imports
  
  # Get imports from RoUS
  SoI_uses_intermediate <- getStateUsebyType(model,type="intermediate", domestic=TRUE, RoUS=FALSE)
  SoI_uses_final <- getStateUsebyType(model,type="final", domestic=TRUE, RoUS=FALSE)
  SoI_uses <- SoI_uses_intermediate + SoI_uses_final
  soi_loc <- getLocation(model,RoUS=FALSE)
  #Remove SoI uses of SoI commodities
  SoI_uses[grep(soi_loc,rownames(SoI_uses)),] <- 0
  import_RoUS <- SoI_uses
  
  # Must be named vector to be used as model demand
  export_RoW<- setNames(export_RoW[,1],row.names(export_RoW))
  export_RoUS <- setNames(export_RoUS[,1],row.names(export_RoUS))
  import_RoUS <- setNames(import_RoUS[,1],row.names(import_RoUS))
  
  CBE_export_RoUS <- calculateStateCBE(model,demand=export_RoUS,domestic=TRUE,RoUS=FALSE, household_emissions=FALSE)
  CBE_export_RoW <- calculateStateCBE(model,demand=export_RoW,domestic=FALSE,RoUS=FALSE, household_emissions=FALSE)
  
  CBE_import_RoUS <- calculateStateCBE(model,demand=import_RoUS,domestic=FALSE,RoUS=FALSE, household_emissions=FALSE)

  # CBE for RoW imports is calculated differently - its done as the difference between total CBE and domestic CBE
  CBE_SoI <-  calculateStateCBE(model,demand="Consumption",domestic=FALSE,RoUS=FALSE, household_emissions=FALSE)
  CBE_SoI_domestic <-  calculateStateCBE(model,demand="Consumption",domestic=TRUE,RoUS=FALSE, household_emissions=FALSE)
  CBE_import_RoW <- CBE_SoI - CBE_SoI_domestic
  
  # Make CBE from imports negative
  CBE_import_RoUS <- -CBE_import_RoUS
  CBE_import_RoW <- -CBE_import_RoW
  
  CBE_trade <- data.frame(cbind(CBE_export_RoUS,CBE_export_RoW,CBE_import_RoUS,CBE_import_RoW))
  colnames(CBE_trade) <- c("export_RoUS","export_RoW","import_RoUS","import_RoW")
  CBE_trade$Balance <- rowSums(CBE_trade)
  return(CBE_trade)
}

# Calculate the share of household emissions for mobile and stationary applications
# Returns a matrix with 1 column and 2 rows (sum to 1)
calculateHouseholdShares <- function(model, indicator) {
  # extract the satellite spec based on the indicator name
  for (s in model$specs$SatelliteTable) {
    if (s$FullName == indicator) {
      sat_spec <- s
    }
  }
  code_loc <- model$specs$ModelRegionAcronyms[[1]]
  ### Regenerate tbs for households to obtain MetaSources
  tbs <- useeior:::generateTbSfromSatSpec(sat_spec, model)
  tbs <- useeior:::conformTbStoStandardSatTable(tbs)
  tbs <- useeior:::conformTbStoIOSchema(tbs, sat_spec, model, agg_metasources=FALSE)
  tbs$Flow <- apply(tbs[, c("Flowable", "Context", "Unit")], 1, FUN = useeior:::joinStringswithSlashes)
  
  df <- subset(tbs, (startsWith(tbs$Sector, "F010") & 
                       tbs$Location == code_loc))
  # unique(df$MetaSources)
  df <- df %>% 
    mutate(
      Sector = case_when(
        grepl('transport', MetaSources) ~ "F010-Mobile",
        grepl('mobile', MetaSources) ~ "F010-Mobile",
        grepl('EPA_GHGI_T_A_97', MetaSources) ~ "F010-Mobile", # HFCs from Transportation
        grepl('EPA_GHGI_T_3_1', MetaSources) ~ "F010-Mobile", # 3-13, 3-14, and 3-15 for mobile emissions
        .default = "F010-Stationary"
      )
    )
  # reshape as matrix and convert to LCIA  
  matrix <- reshape2::dcast(df, Flow ~ Sector, fun.aggregate = sum, value.var = "FlowAmount")
  rownames(matrix) <- matrix$Flow
  matrix$Flow <- NULL
  matrix[setdiff(rownames(model$B), rownames(matrix)), ] <- 0
  matrix <- matrix[rownames(model$B), ]
  lcia <- t(model$C %*% as.matrix(matrix))
  lcia <- sweep(lcia, 2, colSums(lcia), `/`)
  return(lcia)
}

# Calculate N matrix, not created by default w/ import factors
calculateNMatrix <- function(model, state) {
  loc <- paste0("US-", state)
  year <- toString(model$specs$IOYear)
  result <- calculateEEIOModel(model, demand = "Consumption", perspective="FINAL", location = loc)
  N_df <- as.data.frame(reshape2::melt(t(result[[2]])))
  colnames(N_df) <- c("Indicator", "Sector", "Value")
  demand_total <- model[["DemandVectors"]][["vectors"]][[paste0(year, "_", loc, "_Consumption_Complete")]]
  demand_domestic <- model[["DemandVectors"]][["vectors"]][[paste0(year, "_", loc, "_Consumption_Domestic")]]
  demand_imports <- demand_total - demand_domestic
  ## Note demand_imports only has values assigned to SoI
  
  N_df <- merge(N_df, demand_total, by.x = "Sector", by.y=0)
  N_df <- merge(N_df, demand_domestic, by.x = "Sector", by.y=0, suffixes=c("", "_d"))
  N_df <- merge(N_df, demand_imports, by.x = "Sector", by.y=0, suffixes=c("", "_m"))
  N_df["N_coeff"] <- N_df["Value"] / N_df["y"]
  N_df["N_coeff"][is.na(N_df["N_coeff"])] <- 0
  mat <- as.matrix(N_df["N_coeff"])
  rownames(mat) <- N_df[["Sector"]]
  mat <- t(as.matrix(mat[match(colnames(model[["D"]]), rownames(mat)),]))
  rownames(mat) <- "Greenhouse Gases"
  model[["N"]] <- mat
  return(model)
}
