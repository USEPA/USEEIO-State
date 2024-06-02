# StateEEIOFigures.R
library(ggplot2)

#' Prepare a dataframe for graphing from list of 50 state models
#' @param model_list List of completed EEIO models of all 50 states
#' @param matrix_name Name of model matrix to extract data from, e.g. "B", set to NULL to use result vector
#' @param perspective Result perspective "DIRECT" or "FINAL", only used if matrix_name = NULL
#' @param indicator Row name in the specified matrix for the figure
#' @param sector_to_remove Code of one or more BEA sectors that will be removed from the plot. Can be "".
#' @param y_title The title of y axis, excluding unit.
#' @param domestic, bool, TRUE to use domestic results.
#' @param demand, str, "Production" or "Consumption"
#' @param combine_SoIRoUS, TRUE to aggregate results from RoUS and SOI
#' @param household_emissions, pass through to calculateEEIOModel
prepareDFforFigure <- function(model_list, matrix_name=NULL, perspective=NULL,
                               indicator, sector_to_remove="", y_title="", domestic=FALSE, 
                               demand="Production", combine_SoIRoUS=FALSE, household_emissions=FALSE) {
  # Prepare data frame for plot
  df <- data.frame()
  for (modelname in names(model_list)) {
    model <- model_list[[modelname]]
    # Generate BEA sector color mapping
    mapping <- getBEASectorColorMapping(model)
    mapping$SummaryCode <- toupper(mapping$SummaryCode)
    mapping$GroupName <- mapping$SectorName
    # Generate matrix or result
    if (is.null(matrix_name)){
      result <- calculateEEIOModel(model, perspective = perspective, demand = demand,
                                   location=modelname, use_domestic_requirements = domestic,
                                   household_emissions = household_emissions)
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
      # rownames(matrix) <- y_title
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



#' Plot specified matrix coefficients as box and whisker plot
#' @param model_list List of completed EEIO models of all 50 states
#' @param matrix_name Name of model matrix to extract data from, e.g. "B"
#' @param indicator Row name in the specified matrix
#' @param sector_to_remove Code of one or more BEA sectors that will be removed from the plot. Can be "".
#' @param y_title The title of y axis, excluding unit.
#' @param xlim optional upper limit for X-axis.
#' @param scale, int, number of digits to remove from x-axis
#' @export
plotTwoRegionBoxWhisker <- function(model_list, matrix_name, indicator, sector_to_remove="", y_title,
                                    xlim=NA, scale=0) {

  df <- prepareDFforFigure(model_list=model_list, matrix_name=matrix_name, 
                           perspective=NULL, indicator=indicator,
                           sector_to_remove=sector_to_remove, y_title=y_title)
  # drop the RoUS coefficient
  df <- subset(df, df$region != "RoUS")
  df_wide <- reshape2::dcast(df, Indicator + Sector + color + GroupName + SectorName ~ modelname, value.var = "Value")
  df <- reshape2::melt(df_wide, id.vars = c("Indicator", "Sector", "color", "GroupName", "SectorName"),
                       variable.name = "modelname", value.name = "Value")
  df <- df[order(df$GroupName), ]
  label_colors <- rev(unique(df[, c("SectorName", "color")])[, "color"])
  df$x <- df$SectorName

  df <- df[complete.cases(df), ]
  df$Value = df$Value / 10^scale
  
  # plot
  # https://ggplot2.tidyverse.org/reference/geom_boxplot.html
  
  p <- ggplot(df, aes(x = Value,
                      y = factor(x, levels = rev(unique(x))),
                      fill = GroupName,
                      ))

  p <- p + geom_boxplot(outlier.size = 1) +
    scale_fill_manual(values = unique(rev(label_colors))) +
    labs(x = y_title,
         y = element_blank()) +
    theme(axis.text = element_text(color = "black", size = 15),
          axis.text.y = element_text(size = 10, color = label_colors),
          axis.title.x = element_text(size = 10), legend.title = element_blank(),
          legend.justification = c(1, 1), axis.ticks = element_blank(),
          panel.grid.minor.y = element_blank(), plot.margin = margin(c(5, 20, 5, 5)))
  if(!is.na(xlim)){
    p <- p + xlim(0, xlim)
  }

  return(p)
}

#' Plot results for specified sector on map
#' @param model_list List of completed EEIO models of all 50 states
#' @param indicator Row name in the specified matrix for the line plot
#' @param sector The selected sector to highlight in the map, NULL to return all sectors and subset later.
#' @param demand, e.g., "Consumption" or "Production"
#' @param matrix_name, e.g., "N", use NULL if running a demand vector
plotMapResults <- function(model_list, indicator, sector=NULL, demand="Consumption", matrix_name=NULL) {
  df <- prepareDFforFigure(model_list=model_list, matrix_name=matrix_name, perspective="DIRECT",
                           indicator=indicator, sector_to_remove="", demand=demand,
                           combine_SoIRoUS=FALSE)
  if(!is.null(sector)){
    df <- subset(df, df$Sector == sector)    
  }
  if(is.null(matrix_name)) {
    df['perspective'] = 'DIRECT'
    df['demand_type'] = demand
    df2 <- prepareDFforFigure(model_list=model_list, matrix_name=matrix_name, perspective="FINAL",
                              indicator=indicator, sector_to_remove="", demand=demand,
                              combine_SoIRoUS=FALSE)
    df2['perspective'] = 'FINAL'
    df2['demand_type'] = demand
    if(!is.null(sector)) {
      df2 <- subset(df2, df2$Sector == sector)
    }
    df_combined <- rbind(df, df2)
  } else {
    df_combined <- df
  }
  
  df_combined$state <- gsub("US-", "", df_combined$modelname)
  return(df_combined)
  
  # https://www.storybench.org/plot-state-state-data-map-u-s-r/
  # states <- read.csv(file.path("../../data/state_lat_long.csv"), header=TRUE, stringsAsFactors=FALSE)
  # # states <- read.csv(file.path("data/state_lat_long.csv"), header=TRUE, stringsAsFactors=FALSE)
  # states["state"] <- paste0("US-", states$state)
  # df2 <- merge(df2, states, by.x = 'modelname', by.y = 'state')
  # 
  # # plot
  # devtools::install_github("wmurphyrd/fiftystater")
  # data("fifty_states")
  # p <- ggplot() + geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group),color="white", fill="grey92") + 
  #       geom_point(data=df2, aes(x=lon, y=lat, size = Value), color="black") + 
  #       scale_size(name="") + 
  #       guides(size=guide_legend(paste(y_title, sector, sep="-"))) +
  #       theme_void()
  # 
  # return(p)
}


#' Stacked bar chart showing location of impact as SoI or RoUS
#' @param model_list List of completed EEIO models of all 50 states
#' @param state str, state to include
#' @param indicator A vector of indicators to plot
#' @param scale, int, number of digits to remove from x-axis
#' @param household_emissions, pass through to calculateEEIOModel
stackedBarChartResult <- function(model_list, state, indicator, scale=0, demand="Consumption",
                                  household_emissions=FALSE) {
  # DIRECT perspective
  df1 <- prepareDFforFigure(model_list=model_list, matrix_name=NULL, perspective="DIRECT",
                            indicator=indicator, sector_to_remove="",
                            y_title=indicator, combine_SoIRoUS=FALSE, demand=demand,
                            household_emissions=household_emissions)
  df1$Type <- "Total"
  df1d <- prepareDFforFigure(model_list=model_list, matrix_name=NULL, perspective="DIRECT",
                             indicator=indicator, sector_to_remove="",
                             y_title=indicator, combine_SoIRoUS=FALSE, domestic=TRUE, demand=demand,
                             household_emissions=household_emissions)
  df1d$Type <- "Domestic"
  df1 <- rbind(df1, df1d)
  df1 <- reshape2::dcast(data = df1, formula = GroupName+Sector+modelname+region+color+SectorName ~ Type,
                         fun.aggregate = mean, value.var = "Value")
  df1$Type <- "DIRECT"
  
  # FINAL perspective
  df2 <- prepareDFforFigure(model_list=model_list, matrix_name=NULL, perspective="FINAL",
                            indicator=indicator, sector_to_remove="",
                            y_title=indicator, combine_SoIRoUS=FALSE, demand=demand,
                            household_emissions=household_emissions)
  df2$Type <- "Total"
  df2d <- prepareDFforFigure(model_list=model_list, matrix_name=NULL, perspective="FINAL",
                             indicator=indicator, sector_to_remove="",
                             y_title=indicator, combine_SoIRoUS=FALSE, domestic=TRUE, demand=demand,
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
#' @param perspective, "DIRECT", "FINAL" or NULL, if NULL both are shown
#' @param sector_to_remove Code of one or more BEA sectors that will be removed from the plot. Can be "". 
#' @param sector, str options are "sector" for model sectors or "group" to use "GroupName"
stackedBarChartResultFigure <- function(df, x_title, perspective=NULL, sector_to_remove="",
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
  
  if(!is.null(perspective)) {
    df <- subset(df, df$Type == perspective)
  }
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
                # legend.justification = c(1, 1), axis.ticks = element_blank(),
                # panel.grid.minor.y = element_blank(), plot.margin = margin(c(5, 20, 5, 5))
                )
  
  if(is.null(perspective)) {
    p <- p + facet_wrap(~Type, scales = "free_y")
  } else {
    p <- p + ggtitle(paste(stringr::str_to_title(perspective), "Perspective"))
  }
  
  return(p)
}



#' SMM tool like heatmap showing ranking of sectors for select states
#' @param model_list List of completed EEIO models of all 50 states
#' @param states List of states to include
#' @param indicator str, indicator to plot, NULL to include all indicators for single state
#' @param N_sector A numeric value indicating number of sectors to show in the ranking
heatmapStateSectorRanking <- function(model_list, states, indicator=NULL, N_sector) {
  
  df <- prepareDFforFigure(model_list=model_list, matrix_name=NULL, perspective="FINAL",
                           indicator=indicator, sector_to_remove="",
                           y_title="", combine_SoIRoUS=TRUE)
  df <- subset(df, df$modelname %in% states)

  # Reshape df
  if(is.null(indicator)){
    ## NOTE THIS OPTION FOR SINGLE STATE SHOULD BE SUPERSEDED BY USEEIOR FUNCTION
    ## heatmapSectorRanking
    
    # Single state, multiple indicators
    indicators <- levels(df$Indicator)
    # Drop jobs and value add which have positive implications (more = good)
    indicators <- indicators[! indicators %in% c("Value Added", "Jobs Supported")]
    df <- df[which(df$Indicator %in% indicators),]
    df_wide <- reshape2::dcast(df, Sector + SectorName + GroupName + color + modelname ~ Indicator, value.var = "Value")
    df_wide[indicators] <- lapply(df_wide[indicators], rank)
    df_wide$Score <- rowSums(df_wide[indicators])
    df_wide$ranking <- rank(-df_wide$Score)
    df_wide <- df_wide[order(df_wide$ranking), ][1:N_sector, ]
    df2 <- reshape2 ::melt(df_wide, id.vars = c("modelname", "Sector", "color", "GroupName", "SectorName", "ranking", "Score"),
                         variable.name = "Indicator", value.name = "Value")
    x_title <- states
    p <- ggplot(df2, aes(x = factor(Indicator, levels = c("Score", indicators)),
                         y = factor(SectorName, levels = rev(unique(SectorName))),
                         fill = Value,
                         ))
  } else {
    # single indicator, multiple states
    df_wide <- reshape2::dcast(df, Sector + SectorName + GroupName + color + Indicator ~ modelname, value.var = "Value")
    df_wide$Score <- rowSums(df_wide[states])
    df_wide$ranking <- rank(-df_wide$Score)
    df_wide <- df_wide[order(df_wide$ranking), ][1:N_sector, ]
    df2 <- reshape2::melt(df_wide, id.vars = c("Indicator", "Sector", "color", "GroupName", "SectorName", "ranking", "Score"),
                          variable.name = "modelname", value.name = "Value")
    x_title <- indicator
    p <- ggplot(df2, aes(x = factor(modelname, levels = c("Value", states)),
                         y = factor(SectorName, levels = rev(unique(SectorName))),
                         fill = Value,
                         ))
  }

  # Prepare axis label color
  label_colors <- rev(unique(df2[, c("SectorName", "color")])[, "color"])

  # plot
  p <- p +
    geom_tile(color = "black", size = 0.2) +
    scale_fill_gradient(low = "white", high = "red") +
    scale_x_discrete(expand = c(0, 0), position = "top",
                     labels = function(x) stringr::str_wrap(x, 25)) +
    scale_y_discrete(expand = c(0, 0), labels = function(x) stringr::str_wrap(x, 45)) +
    labs(x = x_title, y = "") + theme_bw() +
    theme(axis.text = element_text(color = "black", size = 15),
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(angle = 45, hjust = 0, vjust = 1),
          axis.text.y = element_text(size = 15, color = label_colors),
          legend.position = "none", axis.ticks = element_blank(),
          plot.margin = margin(c(5, 50, 5, 5))) #top, right, bottom, left
  return(p)
}

#' Plot time series for two region models
#' @param model_ls A list of lists: for various years, a list of 50 state models for that year
#' @param state State to include
#' @param indicator A string specifying an indicator to plot
#' @param perspective Result perspective "DIRECT" or "FINAL", only used if matrix_name = NULL
#' @param demand, str, "Production" or "Consumption"
#' @param scale, int, number of digits to remove from y-axis
#' @param household_emissions
twoRegionTimeSeriesPlot <- function(model_ls, state, indicator, plot_per_row = 4,
                                    perspective, demand, scale=0, household_emissions=FALSE) {
  
  dframe <- data.frame()
  for(year in names(model_ls)){
    yr_ls <- model_ls[[as.character(year)]]
    
    df <- prepareDFforFigure(model_list=yr_ls, matrix_name=NULL, perspective=perspective,
                             indicator=indicator, sector_to_remove="", demand=demand,
                             y_title="", combine_SoIRoUS=FALSE, household_emissions=household_emissions)
    df["Year"] <- as.character(year)
    dframe <- rbind(dframe, df)
  }
  dframe <- subset(dframe, dframe$modelname == state)
  # Aggregate SoI and RoUS
  df_agg <- dplyr::group_by(dframe, modelname, Year, Indicator, color, GroupName)
  df_agg <- dplyr::summarize(
    df_agg,
    ValueAgg = sum(Value),
    .groups = 'drop'
  )
  colnames(df_agg)[colnames(df_agg)=="ValueAgg"] <- "Value"
  df_agg$State <- gsub("US-", "", df_agg$modelname)
  df_agg$Value <- df_agg$Value / 10^scale
  return(df_agg)
}



# Stacked bar plots
# Similar to barplotInterregionalTradebyCommodity in VisualizeStateIOresults.R
#' @param plot_per_row Integer specifying how many state plots per row
barplot2RTimeSeries <- function( df,
                                 value_col,
                                 plot_per_row,
                                 facet_by,
                                 with_labels = TRUE,
                                 legend_ncol = 1,
                                 legend_inside = FALSE,
                                 highlight_states = NULL) {

  # Load visualization elements
  vizElements <- loadVisualizationElementsForTimeSeriesPlot()

  # Unpack VizElements
  MasterCrosswalk <- vizElements$MasterCrosswalk
  VisualizationEssentials <- vizElements$VisualizationEssentials
  ColorLabelMapping <- vizElements$ColorLabelMapping
  mapping <- vizElements$mapping
  palette_states <- vizElements$palette_states
  barplot_theme <- vizElements$barplot_theme
  
  df <- subset(df, df$Value >= 0)
  # Plot
  if (facet_by == "state") {
    plotparameters <- ColorLabelMapping[ColorLabelMapping$V1 %in% df$GroupName, ]
    labels <- c(as.character(plotparameters$V1))
    breaks <- c(as.character(plotparameters$V2))
    p <- ggplot(df, aes(x = Year, y = Value, fill = GroupName))
    p <- p + geom_bar(stat = "identity", width = 0.8, color = "white") +
      # scale_x_continuous(breaks = unique(df$Year))# +
      scale_fill_manual(name = "",
                        # breaks = breaks,
                        values = plotparameters$color,
                        # labels = labels
                        ) +
      labs(x = "", y = "") +
      barplot_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(fill = guide_legend(ncol = legend_ncol)) +
      facet_wrap(~State, ncol = plot_per_row)

  } else if (facet_by == "line") {
    p <- ggplot(df, aes(x = Year, y = Value, group = GroupName, color = GroupName)) +
      geom_line(stat = "identity", size=2) +
      # scale_x_continuous(breaks = unique(df$Year))# +
      # scale_fill_manual(name = "",
      #                   # breaks = breaks,
      #                   values = plotparameters$color,
      #                   # labels = labels
      # ) +
      labs(x = "", y = "") +
      barplot_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(fill = guide_legend(ncol = legend_ncol)) +
      facet_wrap(~State, ncol = plot_per_row)

  } else {
    print('Error')
    return()
  }
  return(p)
}

# Taken from VisualizeStateIOresults.R global objects
loadVisualizationElementsForTimeSeriesPlot <- function(){
  vizElements <- list()
  
  vizElements$MasterCrosswalk <- useeior::MasterCrosswalk2012
  configfile <- system.file("extdata", "VisualizationEssentials.yml", package = "useeior")
  vizElements$VisualizationEssentials <- configr::read.config(configfile)
  vizElements$ColorLabelMapping <- as.data.frame(t(cbind.data.frame(vizElements$VisualizationEssentials$BEASectorLevel$ColorLabelMapping)))
  vizElements$ColorLabelMapping$color <- rownames(vizElements$ColorLabelMapping)
  vizElements$mapping <- unique(vizElements$MasterCrosswalk[, c("BEA_2012_Sector_Code", "BEA_2012_Summary_Code", "BEA_2012_Detail_Code")])
  colnames(vizElements$mapping) <- c("Sector", "Summary", "Detail")
  vizElements$mapping <- vizElements$mapping[vizElements$mapping$Sector %in% vizElements$ColorLabelMapping$V2, ]
  vizElements$palette_states <- data.frame(cbind(state.name,
                                                 vizElements$VisualizationEssentials$StateColorPalette),
                               stringsAsFactors = FALSE)
  colnames(vizElements$palette_states) <- c("State", "Color")
  
  
  # Define bar plot common theme
  vizElements$barplot_theme <- theme_linedraw() +
    theme(
      # axis
      axis.text = element_text(color = "black", size = 25),
      axis.title = element_text(size = 30),
      axis.ticks = element_blank(),
      # legend
      legend.title = element_blank(),
      legend.text = element_text(size = 20),
      legend.key.size = unit(0.8, "cm"),
      legend.position = "bottom",
      # panel grid
      panel.spacing = unit(1, "cm"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      # facet strip
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(colour = "black", size = 30)
    )
  
  return(vizElements)
}