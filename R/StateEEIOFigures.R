# StateEEIOFigures.R
library(ggplot2)

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

#' Stacked bar chart showing location of impact as SoI or RoUS
#' @param model
#' @param indicator A vector of indicators to plot
#' @param scale, int, number of digits to remove from x-axis
#' @param household_emissions, pass through to calculateEEIOModel
stackedBarChartResult <- function(model_list, indicator, scale=0, demand="Consumption",
                                  household_emissions=FALSE) {
  
  state <- model$specs$ModelRegionAcronyms[[1]]
  model_list <- list()
  model_list[[state]] <- model

  # DIRECT perspective
  df1 <- prepareDFforFigure(model_list=model_list, matrix_name=NULL, perspective="DIRECT",
                            indicator=indicator, sector_to_remove="",
                            combine_SoIRoUS=FALSE, demand=demand,
                            household_emissions=household_emissions)
  df1$Type <- "Total"
  df1d <- prepareDFforFigure(model_list=model_list, matrix_name=NULL, perspective="DIRECT",
                             indicator=indicator, sector_to_remove="",
                             combine_SoIRoUS=FALSE, domestic=TRUE, demand=demand,
                             household_emissions=household_emissions)
  df1d$Type <- "Domestic"
  df1 <- rbind(df1, df1d)
  df1 <- reshape2::dcast(data = df1, formula = GroupName+Sector+modelname+region+color+SectorName ~ Type,
                         fun.aggregate = mean, value.var = "Value")
  df1$Type <- "DIRECT"
  
  # FINAL perspective
  df2 <- prepareDFforFigure(model_list=model_list, matrix_name=NULL, perspective="FINAL",
                            indicator=indicator, sector_to_remove="",
                            combine_SoIRoUS=FALSE, demand=demand,
                            household_emissions=household_emissions)
  df2$Type <- "Total"
  df2d <- prepareDFforFigure(model_list=model_list, matrix_name=NULL, perspective="FINAL",
                             indicator=indicator, sector_to_remove="",
                              combine_SoIRoUS=FALSE, domestic=TRUE, demand=demand,
                             household_emissions=household_emissions)
  df2d$Type <- "Domestic"
  df2 <- rbind(df2, df2d)
  df2 <- reshape2::dcast(data = df2, formula = GroupName+Sector+modelname+region+color+SectorName ~ Type,
                         fun.aggregate = mean, value.var = "Value")
  df2$Type <- "FINAL"
  
  df <- rbind(df1, df2)
  df <- subset(df, df$modelname == state)
  # Calculate contribution from RoW
  df$Imports <- df$Total - df$Domestic
  df <- reshape2::melt(subset(df, select = -c(Total)),
                       id.vars = c("Sector", "modelname", "region", "Type", "color", "SectorName", "GroupName"),
                       value.name='Value')
  df$region <- ifelse(df$variable == "Imports", "RoW", df$region)
  df <- aggregate(Value ~ Sector + modelname + region + Type + color + SectorName + GroupName,
                  data = df, FUN=sum)

  # reorder stacked bars
  df$region <- factor(df$region, levels = c("RoW", "RoUS", state))

  df$Value = df$Value / 10^scale
  return(df)
}

#' Stacked bar chart showing location of impact as SoI or RoUS or RoW
#' @param df, processed from stackedBarChartResult()
#' @param x_title A string specifying desired title on the x-axis
#' @param perspective, "DIRECT", "FINAL"
#' @param sector_to_remove Code of one or more BEA sectors that will be removed from the plot. Can be "". 
#' @param sector, str options are "sector" for model sectors or "group" to use "GroupName"
stackedBarChartResultFigure <- function(df, x_title, perspective, sector_to_remove="",
                                        level="sector") {
  df <- df[order(df$Sector, df$region),]
  # Remove certain sectors
  df <- df[!df$Sector%in%sector_to_remove, ]  

  if(level == "sector") {
    sector = "SectorName"
  } else if(level == "group") {
    sector = "GroupName"
    df <- aggregate(Value ~ modelname + region + Type + color + GroupName,
                    data = df, FUN=sum)
  }
  df <- subset(df, df$Value >= 0)
  
  label_colors <- rev(unique(df[, c(sector, "color")])[, "color"])
  
  df <- subset(df, df$Type == perspective)

  p <- ggplot(df, aes(x = Value, fill = region,
                      y = factor(.data[[sector]], levels = unique(.data[[sector]])))) +
          geom_col() + 
          guides(fill = guide_legend(reverse = TRUE)) + # Swap legend order
          scale_y_discrete(limits=rev) + # Reverse Y-axis
          xlab(x_title) + # Set X-axis title
          scale_x_continuous(expand = c(0, 0)) +
       theme_bw() +
        theme(
                axis.text = element_text(color = "black", size = 12),
                axis.text.y = element_text(size = 10, color = label_colors),
                axis.title.x = element_text(size = 20),
                axis.title.y = element_blank(),
                legend.text = element_text(size = 20),
                legend.title = element_text(size = 20),
                )

  return(p)
}

