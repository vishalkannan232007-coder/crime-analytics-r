# ==============================================================================
# CRIME ANALYSIS MODULE
# Data Driven Crime Analytics System using R
# ==============================================================================

#' Calculate overall high-level crime summary KPIs
#' @param df Cleaned crime dataset
#' @return list of summary KPI metrics
calculate_crime_summary <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      total_crimes = 0,
      most_common_type = "N/A",
      most_affected_area = "N/A",
      most_common_category = "N/A",
      most_common_status = "N/A",
      solved_rate = "0%"
    ))
  }
  
  tot_crimes <- nrow(df)
  
  # Most common crime type
  type_counts <- sort(table(df$Crime_Type), decreasing = TRUE)
  most_type <- if(length(type_counts) > 0) names(type_counts)[1] else "N/A"
  
  # Most affected area
  area_counts <- sort(table(df$Area), decreasing = TRUE)
  most_area <- if(length(area_counts) > 0) names(area_counts)[1] else "N/A"
  
  # Most common crime category
  cat_counts <- sort(table(df$Crime_Category), decreasing = TRUE)
  most_cat <- if(length(cat_counts) > 0) names(cat_counts)[1] else "N/A"
  
  # Most common status & solved rate
  status_counts <- sort(table(df$Status), decreasing = TRUE)
  most_status <- if(length(status_counts) > 0) names(status_counts)[1] else "N/A"
  
  solved_count <- sum(df$Status == "Solved", na.rm = TRUE)
  solved_pct <- paste0(round((solved_count / tot_crimes) * 100, 1), "%")
  
  return(list(
    total_crimes = tot_crimes,
    most_common_type = most_type,
    most_affected_area = most_area,
    most_common_category = most_cat,
    most_common_status = most_status,
    solved_rate = solved_pct
  ))
}

#' Analyze crime trends across temporal dimensions
#' @param df Cleaned crime dataset
#' @return list of trend data frames (by_year, by_month, by_day_of_week, by_hour)
analyze_crime_trends <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      by_year = data.frame(Year = numeric(0), Count = numeric(0)),
      by_month = data.frame(Month = character(0), Count = numeric(0)),
      by_day_of_week = data.frame(Day_of_Week = character(0), Count = numeric(0)),
      by_hour = data.frame(Hour = numeric(0), Count = numeric(0))
    ))
  }
  
  # Trend by Year
  by_year <- as.data.frame(table(Year = df$Year), stringsAsFactors = FALSE)
  colnames(by_year) <- c("Year", "Count")
  by_year$Year <- as.numeric(by_year$Year)
  by_year <- by_year[order(by_year$Year), ]
  
  # Trend by Month
  month_order <- c("January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December")
  by_month <- as.data.frame(table(Month = df$Month), stringsAsFactors = FALSE)
  colnames(by_month) <- c("Month", "Count")
  by_month$Month <- factor(by_month$Month, levels = month_order)
  by_month <- by_month[order(by_month$Month), ]
  by_month$Month <- as.character(by_month$Month)
  
  # Trend by Day of Week
  dow_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  by_dow <- as.data.frame(table(Day_of_Week = df$Day_of_Week), stringsAsFactors = FALSE)
  colnames(by_dow) <- c("Day_of_Week", "Count")
  by_dow$Day_of_Week <- factor(by_dow$Day_of_Week, levels = dow_order)
  by_dow <- by_dow[order(by_dow$Day_of_Week), ]
  by_dow$Day_of_Week <- as.character(by_dow$Day_of_Week)
  
  # Trend by Hour
  by_hour <- as.data.frame(table(Hour = df$Hour), stringsAsFactors = FALSE)
  colnames(by_hour) <- c("Hour", "Count")
  by_hour$Hour <- as.numeric(by_hour$Hour)
  by_hour <- by_hour[order(by_hour$Hour), ]
  
  return(list(
    by_year = by_year,
    by_month = by_month,
    by_day_of_week = by_dow,
    by_hour = by_hour
  ))
}

#' Analyze crime types and categories
#' @param df Cleaned crime dataset
#' @return list containing crime type tables and top 10 breakdown
analyze_crime_types <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      type_table = data.frame(),
      category_table = data.frame(),
      top10_types = data.frame()
    ))
  }
  
  tot <- nrow(df)
  
  # Crime Type breakdown
  type_counts <- as.data.frame(table(Crime_Type = df$Crime_Type), stringsAsFactors = FALSE)
  colnames(type_counts) <- c("Crime_Type", "Count")
  type_counts$Percentage <- round((type_counts$Count / tot) * 100, 2)
  type_counts <- type_counts[order(-type_counts$Count), ]
  
  top10 <- head(type_counts, 10)
  
  # Category breakdown
  cat_counts <- as.data.frame(table(Crime_Category = df$Crime_Category), stringsAsFactors = FALSE)
  colnames(cat_counts) <- c("Crime_Category", "Count")
  cat_counts$Percentage <- round((cat_counts$Count / tot) * 100, 2)
  cat_counts <- cat_counts[order(-cat_counts$Count), ]
  
  return(list(
    type_table = type_counts,
    category_table = cat_counts,
    top10_types = top10
  ))
}

#' Analyze geographic crime distribution by area
#' @param df Cleaned crime dataset
#' @return list containing area frequencies and area x category matrix
analyze_areas <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      area_summary = data.frame(),
      area_category_matrix = data.frame()
    ))
  }
  
  tot <- nrow(df)
  area_df <- as.data.frame(table(Area = df$Area), stringsAsFactors = FALSE)
  colnames(area_df) <- c("Area", "Count")
  area_df$Percentage <- round((area_df$Count / tot) * 100, 2)
  area_df <- area_df[order(-area_df$Count), ]
  
  # Area vs Category cross-tabulation
  cross_tab <- as.data.frame(table(Area = df$Area, Category = df$Crime_Category), stringsAsFactors = FALSE)
  colnames(cross_tab) <- c("Area", "Category", "Count")
  
  return(list(
    area_summary = area_df,
    area_category_matrix = cross_tab
  ))
}

#' Analyze victim demographic distributions
#' @param df Cleaned crime dataset
#' @return list of victim demographic summaries
analyze_victims <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      gender_dist = data.frame(),
      age_group_dist = data.frame(),
      gender_by_type = data.frame(),
      age_stats = list(mean = 0, median = 0, min = 0, max = 0)
    ))
  }
  
  # Gender distribution
  gender_df <- as.data.frame(table(Victim_Gender = df$Victim_Gender), stringsAsFactors = FALSE)
  colnames(gender_df) <- c("Victim_Gender", "Count")
  gender_df$Percentage <- round((gender_df$Count / nrow(df)) * 100, 2)
  
  # Age groups calculation
  ages <- df$Victim_Age
  age_groups <- cut(
    ages,
    breaks = c(-Inf, 17, 30, 45, 60, Inf),
    labels = c("Under 18", "18–30", "31–45", "46–60", "60+"),
    right = TRUE
  )
  
  age_group_df <- as.data.frame(table(Age_Group = age_groups), stringsAsFactors = FALSE)
  colnames(age_group_df) <- c("Age_Group", "Count")
  age_group_df$Percentage <- round((age_group_df$Count / nrow(df)) * 100, 2)
  
  # Gender vs Crime Type
  gender_type_df <- as.data.frame(table(Crime_Type = df$Crime_Type, Victim_Gender = df$Victim_Gender), stringsAsFactors = FALSE)
  colnames(gender_type_df) <- c("Crime_Type", "Victim_Gender", "Count")
  
  age_stats <- list(
    mean = round(mean(ages, na.rm = TRUE), 1),
    median = round(median(ages, na.rm = TRUE), 1),
    min = min(ages, na.rm = TRUE),
    max = max(ages, na.rm = TRUE)
  )
  
  return(list(
    gender_dist = gender_df,
    age_group_dist = age_group_df,
    gender_by_type = gender_type_df,
    age_stats = age_stats
  ))
}

#' Analyze weapon involvement in crime
#' @param df Cleaned crime dataset
#' @return list of weapon frequency and weapon x crime type breakdown
analyze_weapons <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      weapon_freq = data.frame(),
      weapon_by_type = data.frame()
    ))
  }
  
  wep_df <- as.data.frame(table(Weapon = df$Weapon), stringsAsFactors = FALSE)
  colnames(wep_df) <- c("Weapon", "Count")
  wep_df$Percentage <- round((wep_df$Count / nrow(df)) * 100, 2)
  wep_df <- wep_df[order(-wep_df$Count), ]
  
  wep_type_df <- as.data.frame(table(Crime_Type = df$Crime_Type, Weapon = df$Weapon), stringsAsFactors = FALSE)
  colnames(wep_type_df) <- c("Crime_Type", "Weapon", "Count")
  
  return(list(
    weapon_freq = wep_df,
    weapon_by_type = wep_type_df
  ))
}

#' Analyze crime status resolution states
#' @param df Cleaned crime dataset
#' @return data.frame with status counts and percentages
analyze_crime_status <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(Status = character(0), Count = numeric(0), Percentage = numeric(0)))
  }
  
  status_df <- as.data.frame(table(Status = df$Status), stringsAsFactors = FALSE)
  colnames(status_df) <- c("Status", "Count")
  status_df$Percentage <- round((status_df$Count / nrow(df)) * 100, 2)
  status_df <- status_df[order(-status_df$Count), ]
  
  return(status_df)
}
