# ==============================================================================
# DATA PREPROCESSING MODULE
# Data Driven Crime Analytics System using R
# ==============================================================================

#' Detect missing and duplicate issues in raw dataset
#' @param df Raw data frame
#' @return list of before-preprocessing statistics
detect_data_issues <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      total_records = 0,
      missing_values = 0,
      duplicate_records = 0
    ))
  }
  
  # Total missing values across all cells (including empty strings and "NA")
  missing_cnt <- sum(is.na(df) | df == "" | trimws(as.character(df)) == "NA")
  
  # Duplicate rows
  dup_cnt <- sum(duplicated(df))
  
  return(list(
    total_records = nrow(df),
    missing_values = missing_cnt,
    duplicate_records = dup_cnt
  ))
}

#' Preprocess and clean crime dataset
#' @param df Raw data frame
#' @return list containing cleaned dataset, before stats, after stats, and cleaning log
preprocess_data <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    stop("Cannot preprocess empty dataset.")
  }
  
  cleaning_log <- c()
  before_stats <- detect_data_issues(df)
  cleaning_log <- c(cleaning_log, paste("Initial dataset contains", before_stats$total_records, "records," , before_stats$missing_values, "missing values, and", before_stats$duplicate_records, "duplicate rows."))
  
  clean_df <- df
  
  # 1. Remove duplicate records
  if (before_stats$duplicate_records > 0) {
    clean_df <- clean_df[!duplicated(clean_df), ]
    cleaning_log <- c(cleaning_log, paste("Removed", before_stats$duplicate_records, "duplicate records."))
  }
  
  # 2. Trim whitespace & standardize empty strings to NA
  character_cols <- sapply(clean_df, is.character)
  for (col in names(clean_df)[character_cols]) {
    clean_df[[col]] <- trimws(clean_df[[col]])
    clean_df[[col]][clean_df[[col]] == "" | clean_df[[col]] == "NA"] <- NA
  }
  
  # 3. Handle Date field & derive time attributes
  if ("Date" %in% names(clean_df)) {
    # Convert Date using multiple format parsing
    clean_df$Date_Parsed <- suppressWarnings(as.Date(clean_df$Date))
    
    # If standard conversion failed for some, try alternative formats (DD/MM/YYYY, MM/DD/YYYY)
    na_dates <- is.na(clean_df$Date_Parsed) & !is.na(clean_df$Date)
    if (any(na_dates)) {
      clean_df$Date_Parsed[na_dates] <- suppressWarnings(as.Date(clean_df$Date[na_dates], format = "%d/%m/%Y"))
    }
    na_dates_2 <- is.na(clean_df$Date_Parsed) & !is.na(clean_df$Date)
    if (any(na_dates_2)) {
      clean_df$Date_Parsed[na_dates_2] <- suppressWarnings(as.Date(clean_df$Date[na_dates_2], format = "%m/%d/%Y"))
    }
    
    # Fill remaining unparsed dates with current date or drop invalid
    valid_dates <- !is.na(clean_df$Date_Parsed)
    clean_df <- clean_df[valid_dates, ]
    clean_df$Date <- clean_df$Date_Parsed
    clean_df$Date_Parsed <- NULL
    
    cleaning_log <- c(cleaning_log, "Converted Date field to standard YYYY-MM-DD format.")
    
    # Derive Year, Month, Day, Day_of_Week
    clean_df$Year <- as.numeric(format(clean_df$Date, "%Y"))
    clean_df$Month <- format(clean_df$Date, "%B") # Full month name
    clean_df$Day <- as.numeric(format(clean_df$Date, "%d"))
    clean_df$Day_of_Week <- format(clean_df$Date, "%A") # Monday, Tuesday, etc.
  }
  
  # 4. Handle Time & derive Hour
  if ("Time" %in% names(clean_df)) {
    clean_df$Time[is.na(clean_df$Time)] <- "12:00" # Default noon if missing
    # Extract hour number
    hours <- suppressWarnings(as.numeric(substr(clean_df$Time, 1, 2)))
    hours[is.na(hours)] <- 12
    clean_df$Hour <- hours
    cleaning_log <- c(cleaning_log, "Extracted numerical Hour field from Time.")
  } else {
    clean_df$Hour <- 12 # Default
  }
  
  # 5. Handle Victim_Age & Outlier Correction
  if ("Victim_Age" %in% names(clean_df)) {
    clean_df$Victim_Age <- suppressWarnings(as.numeric(clean_df$Victim_Age))
    # Median imputation for NA or invalid age (<0 or >100)
    valid_ages <- clean_df$Victim_Age[!is.na(clean_df$Victim_Age) & clean_df$Victim_Age >= 0 & clean_df$Victim_Age <= 100]
    median_age <- if(length(valid_ages) > 0) round(median(valid_ages)) else 35
    
    invalid_age_mask <- is.na(clean_df$Victim_Age) | clean_df$Victim_Age < 0 | clean_df$Victim_Age > 100
    if (any(invalid_age_mask)) {
      clean_df$Victim_Age[invalid_age_mask] <- median_age
      cleaning_log <- c(cleaning_log, paste("Imputed missing/invalid Victim_Age records with median age (", median_age, ")."))
    }
  } else {
    clean_df$Victim_Age <- 35
  }
  
  # 6. Standardize Categorical Variables (Victim_Gender, Weapon, Status, City, Area, Crime_Type)
  if ("Victim_Gender" %in% names(clean_df)) {
    clean_df$Victim_Gender[is.na(clean_df$Victim_Gender) | clean_df$Victim_Gender == ""] <- "Unknown"
  } else {
    clean_df$Victim_Gender <- "Unknown"
  }
  
  if ("Weapon" %in% names(clean_df)) {
    clean_df$Weapon[is.na(clean_df$Weapon) | clean_df$Weapon == ""] <- "None"
  } else {
    clean_df$Weapon <- "None"
  }
  
  if ("Status" %in% names(clean_df)) {
    clean_df$Status[is.na(clean_df$Status) | clean_df$Status == ""] <- "Under Investigation"
  } else {
    clean_df$Status <- "Under Investigation"
  }
  
  if ("City" %in% names(clean_df)) {
    clean_df$City[is.na(clean_df$City) | clean_df$City == ""] <- "Metropolis"
  } else {
    clean_df$City <- "Metropolis"
  }
  
  if ("Area" %in% names(clean_df)) {
    clean_df$Area[is.na(clean_df$Area) | clean_df$Area == ""] <- "Downtown"
  } else {
    clean_df$Area <- "Downtown"
  }
  
  if ("Crime_Type" %in% names(clean_df)) {
    clean_df$Crime_Type[is.na(clean_df$Crime_Type) | clean_df$Crime_Type == ""] <- "Other Crime"
  } else {
    clean_df$Crime_Type <- "Other Crime"
  }
  
  # 7. Derive Crime_Category if missing
  if (!"Crime_Category" %in% names(clean_df) || any(is.na(clean_df$Crime_Category))) {
    clean_df$Crime_Category <- ifelse(
      clean_df$Crime_Type %in% c("Assault", "Robbery", "Homicide", "Domestic Violence"), "Violent Crime",
      ifelse(clean_df$Crime_Type %in% c("Burglary", "Vehicle Theft", "Vandalism", "Shoplifting", "Arson"), "Property Crime",
      ifelse(clean_df$Crime_Type %in% c("Fraud", "Cybercrime", "Identity Theft", "Embezzlement"), "Financial Crime",
      ifelse(clean_df$Crime_Type %in% c("Narcotics", "Smuggling"), "Drug Offense", "Other Offense")))
    )
    cleaning_log <- c(cleaning_log, "Derived missing Crime_Category field based on Crime_Type mapping.")
  }
  
  # 8. Numeric Latitude and Longitude conversion
  if ("Latitude" %in% names(clean_df)) {
    clean_df$Latitude <- suppressWarnings(as.numeric(clean_df$Latitude))
  }
  if ("Longitude" %in% names(clean_df)) {
    clean_df$Longitude <- suppressWarnings(as.numeric(clean_df$Longitude))
  }
  
  # After stats
  after_stats <- list(
    total_records = nrow(clean_df),
    missing_values = sum(is.na(clean_df)),
    duplicate_records = sum(duplicated(clean_df))
  )
  
  cleaning_log <- c(cleaning_log, paste("Preprocessing complete. Final record count:", after_stats$total_records))
  
  return(list(
    cleaned_data = clean_df,
    before_stats = before_stats,
    after_stats = after_stats,
    cleaning_log = cleaning_log
  ))
}
