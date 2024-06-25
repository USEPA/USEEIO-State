# StateEEIOFigures.R
library(ggplot2)
library(dplyr)

#' Stacked bar chart (e.g., for showing location of impact as SoI or RoUS or RoW)
#' @param df, must include "Sector", "Value" and "ID" columns
stackedBarChartResultFigure <- function(df, model) {
  mapping <- useeior:::getBEASectorColorMapping(model)
  # mapping$SummaryCode <- toupper(mapping$SummaryCode)
  # mapping$GroupName <- mapping$SectorName
  
  mapping <- rbind(mapping,
                   data.frame(Sector = c("F010-Mobile", "F010-Stationary"),
                              SummaryCode = c("F010-Mobile", "F010-Stationary"),
                              color = mapping$color[mapping$Sector=="F010"], 
                              SectorName = c("Households - Mobile", "Households - Stationary"))
  )
  
  df <- merge(df, unique(mapping[, c("Sector", "color", "SectorName")]), by = "Sector")
 
  # Extract primary code in order to set figure stack alignment
  state <- unique(df$ID)[!(unique(df$ID) %in% c("RoUS", "RoW"))]
  df$ID <- factor(df$ID, levels=c("RoW", "RoUS", state))
  df$SectorName <- factor(df$SectorName, levels=unique(mapping$SectorName))
  df <- df[order(df$SectorName),]
  label_colors <- rev(unique(df[, c("Sector", "color")])[, "color"])
  p <- ggplot(df, aes(x = Value, fill = ID, y = SectorName)) +
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
    geom_line(linewidth=1) +
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
                                    model,
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
      geom_line(stat = "identity", linewidth=1) +
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


contributionToImpactBySectorChart <- function(model, sector, indicator, state) {
  if(is.null(model[["N"]])) {
    model <- calculateNMatrix(model, state)
  }
  df0 <- useeior::disaggregateTotalToDirectAndTier1(model, indicator)
  
  sector_codes <- c(paste0(sector, "/US-", state), paste0(sector, "/RoUS"))
  df <- subset(df0, df0$sector_code %in% sector_codes)
  
  # Reaggregate ignoring location of purchased commodity
  df_agg <- df %>%
    group_by(purchased_commodity) %>%
    summarize(
      impact_per_purchase = sum(impact_per_purchase),
      .groups = 'drop'
    )
  
  # Identify the top 5 sectors being purchases
  top5 <- df_agg %>% top_n(5)
  if("Direct" %in% top5$purchased_commodity) {
    top5 <- df_agg %>%
      top_n(6) %>%
      filter(purchased_commodity!='Direct') %>%
      arrange(impact_per_purchase)
  }
  sectors_to_show <- top5[["purchased_commodity"]]
  sectors_to_show <- c(sectors_to_show, "Imports", "Direct")
  
  df <- df %>% 
    mutate(
      tag = case_when(
        purchased_commodity %in% sectors_to_show ~ 1,
        !(purchased_commodity %in% sectors_to_show) ~ 0
      )
    )
  ## Aggregate all other sectors and recombine
  others <- subset(df, df$tag == 0)
  others <- aggregate(impact_per_purchase ~ sector_code, data = others, FUN=sum)
  others$purchased_commodity <- "Other"
  others$purchased_commodity_code <- "Other"
  df <- subset(df, df$tag == 1)
  df[is.na(df)] <- "Direct"
  common_cols <- intersect(colnames(df), colnames(others))
  df <- rbind(df[, common_cols], others[, common_cols])
  
  # Reaggregate ignoring location of purchased commodity
  df <- df %>%
    group_by(sector_code, purchased_commodity) %>%
    summarize(
      impact_per_purchase = sum(impact_per_purchase),
      .groups = 'drop'
    )
  
  import_row <- data.frame(sector_code = "RoW", purchased_commodity = "Imports",
                           impact_per_purchase = model$N_m[indicator, paste0(sector, "/US-", state)])
  df <- rbind(df, import_row)    

  name <- model$Commodities[model$Commodities$Code == sector, "Name"][1]
  # Rename sectors to states
  df['sector_code'] <- data.frame(lapply(df['sector_code'], function(x) {
    gsub(sector_codes[1], state, x)}))
  df['sector_code'] <- data.frame(lapply(df['sector_code'], function(x) {
    gsub(sector_codes[2], "RoUS", x)}))
  
  ## Order sectors for figure
  df$sector_code <- factor(df$sector_code, levels=c(state, "RoUS", "RoW"))
  df$purchased_commodity <- factor(df$purchased_commodity, levels=c("Other", sectors_to_show))
  levels(df$purchased_commodity) <- str_wrap(levels(df$purchased_commodity), 40)
  p <- ggplot(df, aes(fill=purchased_commodity, x = sector_code, y=impact_per_purchase)) +
    geom_bar(position="stack", stat = "identity") +
    xlab(element_blank()) +
    ylab("kg CO2e / $ produced") + 
    theme_bw() + 
    labs(fill="Source") +
    theme(text = element_text(size=18)) +
    scale_y_continuous(expand = c(0, 0))
  
  return(p)
}

# extract a legend, for use in multi-pane figures
# see https://stackoverflow.com/a/13650878
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
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
