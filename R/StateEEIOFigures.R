# StateEEIOFigures.R
library(ggplot2)

#' Stacked bar chart showing location of impact as SoI or RoUS
#' @param model
#' @param indicator A vector of indicators to plot
#' @param scale, int, number of digits to remove from x-axis
#' @param demand, str, "Consumption" or "Production"
#' @param perspective, str, "FINAL" or "DIRECT"
#' @param household_emissions, pass through to calculateEEIOModel
stackedBarChartResult <- function(model_list, indicator, scale=0, demand="Consumption",
                                  perspective, household_emissions=FALSE) {
  
  state <- model$specs$ModelRegionAcronyms[[1]]
  model_list <- list()
  model_list[[state]] <- model

  df1 <- prepareDFforFigure(model_list=model_list, matrix_name=NULL, perspective=perspective,
                            indicator=indicator, sector_to_remove="",
                            combine_SoIRoUS=FALSE, demand=demand,
                            household_emissions=household_emissions)
  df1$Type <- "Total"
  df1d <- prepareDFforFigure(model_list=model_list, matrix_name=NULL, perspective=perspective,
                             indicator=indicator, sector_to_remove="",
                             combine_SoIRoUS=FALSE, domestic=TRUE, demand=demand,
                             household_emissions=household_emissions)
  df1d$Type <- "Domestic"
  df1 <- rbind(df1, df1d)
  df <- reshape2::dcast(data = df1, formula = GroupName+Sector+modelname+region+color+SectorName ~ Type,
                         fun.aggregate = mean, value.var = "Value")
  df$Type <- perspective
  
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

#' Stacked bar chart (e.g., for showing location of impact as SoI or RoUS or RoW)
#' @param df, must include "Sector", "Value" and "ID" columns
stackedBarChartResultFigure <- function(df, model) {
  mapping <- useeior:::getBEASectorColorMapping(model)
  # mapping$SummaryCode <- toupper(mapping$SummaryCode)
  # mapping$GroupName <- mapping$SectorName
  
  df <- merge(df, unique(mapping[, c("Sector", "color", "SectorName")]), by = "Sector")
 
  label_colors <- rev(unique(df[, c("Sector", "color")])[, "color"])
  p <- ggplot(df, aes(x = Value, fill = ID,
                      y = factor(.data[["SectorName"]], levels = unique(.data[["SectorName"]])))) +
          geom_col() + 
          guides(fill = guide_legend(reverse = TRUE)) + # Swap legend order
          scale_y_discrete(limits=rev) + # Reverse Y-axis
          scale_x_continuous(expand = c(0, 0)) +
       theme_bw() +
        theme(
                axis.text = element_text(color = "black", size = 12),
                axis.text.y = element_text(size = 10, color = label_colors),
                axis.title.x = element_text(size = 12),
                axis.title.y = element_blank(),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 12),
                )

  return(p)
}


#Plots a line chart for a StateResult where x is time in years and y is variable
# ylabel
lineChartFigure <- function(result,ylabel) {
  df_figure <- reformatWidetoLong(result)
  p <- ggplot(df_figure, aes(x = Year, y = value, group=variable, color=variable))+
    geom_line(size=3) +
    ylab(ylabel) +
    xlab("Year") +
    scale_colour_hue(name='Source') +
    theme_bw() +
    theme(text = element_text(size=20))
  return(p)
}

#' Plot time series for two region models
#' @param df
#' @param plottype, str, "line" or "bar"
#' @param legend_ncol
twoRegionTimeSeriesPlot <- function(df,
                                    plottype, # bar or line
                                    legend_ncol = 2) {
  df_figure <- reformatWidetoLong(df)
  mapping <- useeior:::getBEASectorColorMapping(model)
  # mapping$SummaryCode <- toupper(mapping$SummaryCode)
  # mapping$GroupName <- mapping$SectorName
  
  df_figure <- merge(df_figure, unique(mapping[, c("Sector", "color", "SectorName")]),
                     by.x = "variable", by.y = "Sector")

  # Load visualization elements
  vizElements <- loadVisualizationElementsForTimeSeriesPlot()
  ColorLabelMapping <- vizElements$ColorLabelMapping
  barplot_theme <- vizElements$barplot_theme
  
  df_figure <- subset(df_figure, df_figure$value >= 0)
  # Plot
  plotparameters <- ColorLabelMapping[ColorLabelMapping$V1 %in% df_figure$SectorName, ]
  if (plottype == "bar") {
    p <- ggplot(df_figure, aes(x = Year, y = value, fill = SectorName))
    p <- p + geom_bar(stat = "identity", width = 0.8, color = "white") +
      scale_fill_manual(name = "", values = plotparameters$color) +
      labs(x = "", y = "") +
      barplot_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(fill = guide_legend(ncol = legend_ncol))

  } else if (plottype == "line") {
    p <- ggplot(df_figure, aes(x = Year, y = value, group = SectorName, color = SectorName)) +
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
