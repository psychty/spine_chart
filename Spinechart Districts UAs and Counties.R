



library(readr)
library(dplyr)
library(ggplot2)

LAD <- read_csv(url("https://opendata.arcgis.com/datasets/a267b55f601a4319a9955b0197e3cb81_0.csv"))
Counties <- read_csv(url("https://opendata.arcgis.com/datasets/7e6bfb3858454ba79f5ab3c7b9162ee7_0.csv"))

lookup <- read_csv(url("https://opendata.arcgis.com/datasets/41828627a5ae4f65961b0e741258d210_0.csv")) # This is a lower tier LA to upper tier LA lookup
UA <- subset(lookup, LTLA17NM == UTLA17NM)
Region <- read_csv(url("https://opendata.arcgis.com/datasets/cec20f3a9a644a0fb40fbf0c70c3be5c_0.csv"))

colnames(Region) <- c("Area_Code", "Area_Name", "Area_Name_Welsh", "FID")
Region$Area_Type <- "Region"
Region <- Region[c("Area_Code", "Area_Name", "Area_Type")]

LAD <- subset(LAD, substr(LAD$LAD17CD, 1, 1) == "E")
LAD$Area_Type <- ifelse(LAD$LAD17NM %in% UA$LTLA17NM, "Unitary Authority", "District")
colnames(LAD) <- c("Area_Code", "Area_Name", "Area_Name_Welsh", "FID", "Area_Type")
LAD <- LAD[c("Area_Code", "Area_Name", "Area_Type")]

Counties$Area_type <- "County"
colnames(Counties) <- c("Area_Code", "Area_Name", "Col2", "Col3", "FID", "Area_Type")
Counties <- Counties[c("Area_Code", "Area_Name", "Area_Type")]

England <- data.frame(Area_Code = "E92000001", Area_Name = "England", Area_Type = "Country")

Areas <- rbind(LAD, Counties, Region, England)
rm(LAD, Counties, Region, England, lookup, UA)




spine_theme = function(){
  theme( 
    legend.position = "none", 
    plot.background = element_rect(fill = "#E7E7E7"), 
    panel.background = element_rect(fill = "#FFFFFF"), 
    axis.text = element_blank(), 
    plot.title = element_text(colour = "#000000", face = "bold", size = 12),     axis.title = element_blank(),     
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    strip.text = element_text(colour = "white"), 
    strip.background = element_rect(fill = "#327d9c"), 
    axis.ticks = element_blank() 
  ) 
} 

spine_function <- function(chosen_indicator, chosen_area, areas_level, comparison, chosen_age, chosen_sex){
  area_tp <- ifelse(areas_level == "District & UA", 101, ifelse(areas_level == "Counties", 102, ifelse(areas_level == "GP", 7, ifelse(areas_level == "CCG", 19, 102))))
  
  if(missing(chosen_indicator) == TRUE) {
    stop("The indicator has not been specified")
  }
  if(missing(chosen_area) == TRUE) {
    stop("The area has not been specified")
  }
  if(missing(areas_level) == TRUE) {
    stop("The indicator has not been specified")
  }
  if(!(areas_level %in% c("District & UA", "Counties", "GP", "CCG"))) {
    stop("The area level must be either 'District & UA', 'Counties', 'GP', or 'CCG'")
  }
  if(missing(comparison) == TRUE) {
    stop("The comparison has not been specified")
  }
  if(missing(chosen_age) == TRUE) {
    stop("The age has not been specified")
  }
  if(missing(chosen_sex) == TRUE) {
    stop("The sex has not been specified")
  }
  
  chosen_data <- read_csv(url(paste0("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=", chosen_indicator,"&child_area_type_id=", area_tp ,"&parent_area_type_id=102&profile_id=", profile_id_x)), col_types = cols(.default = col_character(),    `Indicator ID` = col_integer(), `Time period` = col_integer(),Value = col_double(),    `Lower CI 95.0 limit` = col_double(),`Upper CI 95.0 limit` = col_double(),    Denominator = col_integer(), `Time period Sortable` = col_integer()))
  
  length(unique(chosen_data$`Area Name`))
  unique(chosen_data$`Area Type`)
  
  names(chosen_data)
  
  chose_area <- chosen_data[c("Parent Code","Parent Name","Area Code","Area Name","Area Type","Sex","Age", "Category","Time period", "Value" )]
  
  chose_area <- left_join(chose_area, Areas, by = c("Area Code" = "Area_Code"))
  
  
  View(subset(chose_area, is.na(Area_Type)))
  
  chosen_data_county <- subset(chosen_data, `Area Type` == "County & UA" & is.na(`Category Type`) & `Parent Name` == "England")
  
  # Unitary Authorities will appear twice
  
  chosen_data_england <- subset(chosen_data, `Area Name` == "England" & is.na(`Category Type`))
  
  chosen_data <- subset(chosen_data, `Area Type` == areas_level & `Parent Name` != "England")
  
  chosen_data_region <- read_csv(url(paste0("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=", chosen_indicator,"&child_area_type_id=", area_tp ,"&parent_area_type_id=6&profile_id=", profile_id_x)), col_types = cols(.default = col_character(),    `Indicator ID` = col_integer(), `Time period` = col_integer(),Value = col_double(),    `Lower CI 95.0 limit` = col_double(),`Upper CI 95.0 limit` = col_double(),    Denominator = col_integer(), `Time period Sortable` = col_integer()))
  
  chosen_data_region <- subset(chosen_data_region, `Area Type` == "Region")
  
  chosen_data <- rbind(chosen_data, chosen_data_county, chosen_data_region, chosen_data_england)
  
  rm(chosen_data_county, chosen_data_region, chosen_data_england)
  
  names(chosen_data) <- gsub(" ", "", names(chosen_data))
  
  chosen_data <- subset(chosen_data, is.na(CategoryType))
  
  # Order by descending year
  chosen_data <- arrange(chosen_data, desc(Timeperiod))
  latest_p <- unique(chosen_data$Timeperiod)[1]
  
  # One flaw in the data is that unitary authorities won't be included in any district and unitary authority spinechart because they are only labeled county and unitary authority!!!
  
  chosen_data_latest <- subset(chosen_data, AreaType == areas_level & Timeperiod == latest_p & Age == chosen_age & Sex == chosen_sex)
  
  chosen_trend <<- subset(chosen_data, AreaName == chosen_area & Age == chosen_age & Sex == chosen_sex)
  comparator_trend <<- subset(chosen_data, AreaName == comparison & Age == chosen_age & Sex == chosen_sex)
  
  if (nrow(chosen_trend) == 0) {
    stop("There is no data for the combination of items given.")
  }
  
  statistic <- as.numeric(subset(chosen_data_latest, AreaName == chosen_area, select = "Value"))
  LCI_statistic <- as.numeric(subset(chosen_data_latest, AreaName == chosen_area, select = "LowerCI95.0limit"))
  UCI_statistic <- as.numeric(subset(chosen_data_latest, AreaName == chosen_area, select = "UpperCI95.0limit"))
  
  ch <- subset(chosen_data, AreaName == comparison & Timeperiod == latest_p & Age == chosen_age & Sex == chosen_sex)
  
  comp_statistic <- as.numeric(subset(chosen_data, AreaName == comparison & Timeperiod == latest_p & Age == chosen_age & Sex == chosen_sex, select = "Value"))
  LCI_comp_statistic <- as.numeric(subset(chosen_data, AreaName == comparison & Timeperiod == latest_p & Age == chosen_age & Sex == chosen_sex, select = "LowerCI95.0limit"))
  UCI_comp_statistic <- as.numeric(subset(chosen_data, AreaName == comparison & Timeperiod == latest_p & Age == chosen_age & Sex == chosen_sex, select = "UpperCI95.0limit"))
  
  sig_compare = ifelse(LCI_statistic > UCI_comp_statistic,"sig higher",ifelse(UCI_statistic < LCI_comp_statistic,"sig lower","no diff"))
  
  min_ind <- summary(chosen_data_latest$Value)[[1]]
  Q1_ind <- summary(chosen_data_latest$Value)[[2]]
  median_ind <- summary(chosen_data_latest$Value)[[3]]
  mean_ind <- summary(chosen_data_latest$Value)[[4]]
  Q3_ind <- summary(chosen_data_latest$Value)[[5]]
  max_ind <- summary(chosen_data_latest$Value)[[6]]
  
  
  chosen_meta <- read_csv(url(paste0("https://fingertips.phe.org.uk/api/indicator_metadata/csv/by_indicator_id?indicator_ids=", chosen_indicator)), col_types = cols(.default = col_character(),  `Indicator ID` = col_integer()))
  
  # Area worst/lowest ###
  area_worst_lowest <- ifelse(chosen_meta$Polarity == "RAG - Low is good", max_ind, min_ind)
  # Area best/highest ###
  area_best_highest <- ifelse(chosen_meta$Polarity == "RAG - Low is good", min_ind, max_ind)
  
  # Scaled minimum ###
  scaled_min <- ifelse((median_ind - min_ind) > (max_ind - median_ind), min_ind, median_ind - (max_ind - median_ind))
  # Scaled maximum ###
  scaled_max <- ifelse(scaled_min == min_ind, median_ind + (median_ind - min_ind), max_ind)
  
  # Scaled worst ###
  scaled_worst <- ifelse(chosen_meta$Polarity == "RAG - Low is good", scaled_max, scaled_min)
  # Scaled best ###
  scaled_best <- ifelse(chosen_meta$Polarity == "RAG - Low is good", scaled_min, scaled_max)
  
  # Scaled statistic
  scaled_statistic <- (statistic - scaled_worst)/(scaled_best- scaled_worst)
  
  # Worst
  worst <- ifelse(scaled_worst == 0, NA, ifelse(chosen_meta$Polarity == "RAG - Low is good", (area_worst_lowest - scaled_worst)/(scaled_best - scaled_worst),(min_ind - scaled_worst)/(scaled_best - scaled_worst)))
  
  # Bottom quartile
  bottom_quartile <- ifelse(chosen_meta$Polarity == "RAG - Low is good", (Q3_ind - scaled_worst) / (scaled_best - scaled_worst), (Q1_ind - scaled_worst)/(scaled_best - scaled_worst))
  
  # Top quartile
  top_quartile <- ifelse(chosen_meta$Polarity == "RAG - Low is good", (Q1_ind - scaled_worst) / (scaled_best - scaled_worst), (Q3_ind - scaled_worst)/(scaled_best - scaled_worst))
  
  # Best
  best <- ifelse(chosen_meta$Polarity == "RAG - Low is good", (min_ind - scaled_worst) / (scaled_best - scaled_worst), (max_ind - scaled_worst)/(scaled_best - scaled_worst))
  
  # comparator scaled value
  comparator_statistic <- (comp_statistic - scaled_worst)/(scaled_best- scaled_worst)
  
  # For stacked bars 
  sb_part_0 <- 0 + worst
  sb_part_1 <- bottom_quartile - worst
  sb_part_2 <- top_quartile - bottom_quartile
  sb_part_3 <- best - top_quartile
  
  sb <- data.frame(sb = c(sb_part_0, sb_part_1, sb_part_2, sb_part_3), item = c("blank_space","25th Percentile", "75th Percentile", "Best"))
  
  sb$item <- factor(sb$item, levels = c("Best","75th Percentile", "25th Percentile", "blank_space"))
  
  sb <- arrange(sb, desc(item))
  
  better <- "#3ECC26"
  no_diff <- "#E7AF27"
  worse <- "#CC2629"
  not_applic <- "#8E8E8E"
  higher = "#BED2FF"
  lower = "#5555E6"
  
  colour_sig <- ifelse(chosen_meta$Polarity == "RAG - Low is good" & sig_compare == "no diff", no_diff, ifelse(chosen_meta$Polarity == "RAG - High is good" & sig_compare == "no diff", no_diff,  ifelse(chosen_meta$Polarity == "RAG - Low is good" & sig_compare == "sig higher", worse, ifelse(chosen_meta$Polarity == "RAG - Low is good" & sig_compare == "sig lower", better, ifelse(chosen_meta$Polarity == "RAG - High is good" & sig_compare == "sig higher", better, ifelse(chosen_meta$Polarity == "RAG - High is good" & sig_compare == "sig lower", worse, ifelse(chosen_meta$Polarity == "Not applicable" & sig_compare == "no diff", no_diff, ifelse(chosen_meta$Polarity == "Not applicable" & sig_compare == "sig higher", higher, ifelse(chosen_meta$Polarity == "Not applicable" & sig_compare == "sig lower", lower, not_applic)))))))))
  
  area_label <- gsub(" ","_", chosen_area)
  
  
  # assign(paste0("spinechart_", chosen_indicator, "_", area_label),
  spinechart_w_label <<- ggplot(data=sb, aes(x = "sb", y = sb, fill = factor(item))) +
    geom_bar(stat="identity", width = 0.2) +
    geom_errorbar(aes(ymax=0.5, ymin=0.5), position=position_dodge(), size = 1.2, width = 0.2) + # this adds a vertical line at the mid point 
    geom_point(aes(x= "sb", y = scaled_statistic), colour = "#000000", shape = 21, fill = colour_sig, size = 12) +
    coord_flip() +
    ggtitle(paste(ch$IndicatorName, "; ", chosen_area, ";\n", chosen_sex, "; ", chosen_age, "; ", latest_p, sep = "")) +
    scale_fill_manual(values = c("#ffffff","#C9C9C9","#8B8B8B","#C9C9C9"), limits = c("blank_space", "25th Percentile","75th Percentile","Best"), name = "") +
    scale_y_continuous(expand = c(0.05,0), limits = c(0,1)) +
    geom_text(aes(y = sb_part_0 + sb_part_1 + sb_part_2 + sb_part_3, label = "Best/\nhighest"), size = 2.5, vjust = -2) +
    geom_text(aes(y = sb_part_0 + sb_part_1, label = "25th\nPercentile"), size = 2.5, vjust = 4) +
    geom_text(aes(y = sb_part_0 + sb_part_1 + sb_part_2, label = "75th\nPercentile"), size = 2.5, vjust = 4) +
    geom_text(aes(y = sb_part_0, label = "Worst/\nlowest"), size = 2.5, vjust = -2) +
    spine_theme()
  
  
  spinechart_no_label <<- ggplot(data=sb, aes(x = "sb", y = sb, fill = factor(item))) +
    geom_bar(stat="identity", width = 0.2) +
    geom_errorbar(aes(ymax=0.5, ymin=0.5), position=position_dodge(), size = 1.2, width = 0.2) + # this adds a vertical line at the mid point 
    geom_point(aes(x= "sb", y = scaled_statistic), colour = "#000000", shape = 21, fill = colour_sig, size = 12) +
    coord_flip() +
    ggtitle(paste(ch$IndicatorName, "; ", chosen_area, ";\n", chosen_sex, "; ", chosen_age, "; ", latest_p, sep = "")) +
    scale_fill_manual(values = c("#ffffff","#C9C9C9","#8B8B8B","#C9C9C9"), limits = c("blank_space", "25th Percentile","75th Percentile","Best"), name = "") +
    scale_y_continuous(expand = c(0.05,0), limits = c(0,1)) +
    #geom_text(aes(y = sb_part_0 + sb_part_1 + sb_part_2 + sb_part_3, label = "Best/\nhighest"), size = 2.5, vjust = -2) +
    #  geom_text(aes(y = sb_part_0 + sb_part_1, label = "25th\nPercentile"), size = 2.5, vjust = 4) +
    # geom_text(aes(y = sb_part_0 + sb_part_1 + sb_part_2, label = "75th\nPercentile"), size = 2.5, vjust = 4) +
    #geom_text(aes(y = sb_part_0, label = "Worst/\nlowest"), size = 2.5, vjust = -2) +
    spine_theme()
  
  
}

