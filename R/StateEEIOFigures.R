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
