# StateEEIOCalculations.R

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
  
  return(list("demand" = soi_consumption, "demand_by_source" = total_demand_by_source))
}
  