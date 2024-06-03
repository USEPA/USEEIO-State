# StateEEIOFigures.R
library(ggplot2)

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

lineChartDemand <- function(demand) {
  demand_figure <- melt(demand, id.vars=c('Year'))
  demand_figure$value <- demand_figure$value / 1000000000
  p <- ggplot(demand_figure, aes(x = Year, y = value, group=variable, color=variable))+
    geom_line(size=3) +
    ylab("Billion $") +
    xlab("Year") +
    scale_colour_hue(name='Source') +
    theme_bw() +
    theme(text = element_text(size=20))
  return(p)
}

#' Plot time series for two region models
#' @param df
#' @param plottype, str, "line" or "bar"
#' @param scale, int, number of digits to remove from y-axis
#' @param legend_ncol
twoRegionTimeSeriesPlot <- function(df,
                                    plottype, # bar or line
                                    scale = 0,
                                    legend_ncol = 1) {

  # Aggregate SoI and RoUS
  df_agg <- dplyr::group_by(df, modelname, Year, Indicator, color, GroupName)
  df_agg <- dplyr::summarize(
    df_agg,
    ValueAgg = sum(Value),
    .groups = 'drop'
  )
  colnames(df_agg)[colnames(df_agg)=="ValueAgg"] <- "Value"
  df_agg$State <- gsub("US-", "", df_agg$modelname)
  df_agg$Value <- df_agg$Value / 10^scale
  
  # Load visualization elements
  vizElements <- loadVisualizationElementsForTimeSeriesPlot()
  ColorLabelMapping <- vizElements$ColorLabelMapping
  barplot_theme <- vizElements$barplot_theme
  
  df_agg <- subset(df_agg, df_agg$Value >= 0)
  # Plot
  plotparameters <- ColorLabelMapping[ColorLabelMapping$V1 %in% df$GroupName, ]
  if (plottype == "bar") {
    p <- ggplot(df_agg, aes(x = Year, y = Value, fill = GroupName))
    p <- p + geom_bar(stat = "identity", width = 0.8, color = "white") +
      scale_fill_manual(name = "", values = plotparameters$color) +
      labs(x = "", y = "") +
      barplot_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(fill = guide_legend(ncol = legend_ncol))

  } else if (plottype == "line") {
    p <- ggplot(df_agg, aes(x = Year, y = Value, group = GroupName, color = GroupName)) +
      geom_line(stat = "identity", size=2) +
      # geom_point(stat = "identity", size=2) +
      scale_color_manual(name = "", values = plotparameters$color) +
      labs(x = "", y = "") +
      barplot_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(color = guide_legend(ncol = legend_ncol))

  } else {
    print('Error not an available plottype')
    return()
  }
  return(p)
}


# Taken from VisualizeStateIOresults.R global objects
loadVisualizationElementsForTimeSeriesPlot <- function(){
  vizElements <- list()
  
  configfile <- system.file("extdata", "VisualizationEssentials.yml", package = "useeior")
  vizElements$VisualizationEssentials <- configr::read.config(configfile)
  vizElements$ColorLabelMapping <- as.data.frame(t(cbind.data.frame(vizElements$VisualizationEssentials$BEASectorLevel$ColorLabelMapping)))
  vizElements$ColorLabelMapping$color <- rownames(vizElements$ColorLabelMapping)
  
  # Define bar plot common theme
  vizElements$barplot_theme <- theme_linedraw() +
    theme(
      # axis
      axis.text = element_text(color = "black", size = 14),
      axis.title = element_text(size = 16),
      axis.ticks = element_blank(),
      # legend
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.6, "cm"),
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
