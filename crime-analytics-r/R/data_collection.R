# ==============================================================================
# DATA COLLECTION MODULE
# Data Driven Crime Analytics System using R
# ==============================================================================

# Required core columns for validation
REQUIRED_CRIME_COLUMNS <- c(
  "Crime_ID", "Date", "City", "Area", 
  "Crime_Type", "Crime_Category", "Status"
)

# Optional but recommended columns
RECOMMENDED_CRIME_COLUMNS <- c(
  "Time", "Victim_Age", "Victim_Gender", "Weapon", "Latitude", "Longitude"
)

#' Load built-in sample dataset
#' @return data.frame containing sample crime data
load_sample_data <- function() {
  sample_path <- system.file("data", "sample_crime_data.csv", package = "crimeanalytics")
  if (!file.exists(sample_path)) {
    # Fallback to relative path if not packaged
    sample_path <- "data/sample_crime_data.csv"
  }
  
  if (!file.exists(sample_path)) {
    stop("Sample dataset file 'data/sample_crime_data.csv' not found.")
  }
  
  df <- read.csv(sample_path, stringsAsFactors = FALSE, check.names = FALSE)
  return(df)
}

#' Load uploaded custom CSV dataset
#' @param filepath Path to uploaded CSV file
#' @return data.frame containing custom crime data
load_custom_data <- function(filepath) {
  if (is.null(filepath) || !file.exists(filepath)) {
    stop("Invalid file path or file does not exist.")
  }
  
  tryCatch({
    df <- read.csv(filepath, stringsAsFactors = FALSE, check.names = FALSE)
    return(df)
  }, error = function(e) {
    stop(paste("Failed to read CSV file:", e$message))
  })
}

#' Validate crime dataset columns
#' @param df data.frame to validate
#' @return list containing validation results (valid, missing_cols, message)
validate_crime_data <- function(df) {
  if (is.null(df) || !is.data.frame(df)) {
    return(list(
      valid = FALSE,
      missing_cols = REQUIRED_CRIME_COLUMNS,
      message = "Uploaded dataset is empty or not a valid data frame."
    ))
  }
  
  current_cols <- names(df)
  missing <- setdiff(REQUIRED_CRIME_COLUMNS, current_cols)
  
  if (length(missing) > 0) {
    return(list(
      valid = FALSE,
      missing_cols = missing,
      message = paste("Missing required crime columns:", paste(missing, collapse = ", "))
    ))
  }
  
  return(list(
    valid = TRUE,
    missing_cols = character(0),
    message = "Dataset validation successful! All required columns are present."
  ))
}

#' Extract basic dataset metadata and information
#' @param df data.frame
#' @return list with dataset info metrics
get_dataset_info <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      num_records = 0,
      num_columns = 0,
      column_names = character(0),
      memory_size = "0 KB",
      col_types = character(0),
      missing_cell_count = 0
    ))
  }
  
  mem_bytes <- object.size(df)
  mem_formatted <- format(mem_bytes, units = "auto")
  
  col_types <- sapply(df, class)
  col_types_str <- paste(names(col_types), "(", col_types, ")", collapse = ", ")
  
  total_missing <- sum(is.na(df) | df == "" | df == "NA")
  
  return(list(
    num_records = nrow(df),
    num_columns = ncol(df),
    column_names = colnames(df),
    memory_size = mem_formatted,
    col_types = col_types,
    col_types_summary = col_types_str,
    missing_cell_count = total_missing
  ))
}
