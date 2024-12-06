# ------------------------------------------------------------------------------
# merge all the time series data 

# Step 1: Write a function that will read in all the data ----------------------

read_and_rename_csvs <- function(folder_path) {
  # List all CSV files in the folder
  file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize a list to store the dataframes
  data_list <- list()
  
  for (file in file_list) {
    # Extract the base name (without the folder path)
    file_name <- basename(file)
    
    # Parse the file name to extract "surface/bottom" and the date
    parts <- strsplit(file_name, "_")[[1]]
    layer <- tolower(parts[2])  # "surface" or "bottom"
    month <- as.numeric(substr(parts[3], 1, 2))  # Extract "MM" from "MMDDYY"
    year <- substr(parts[3], 5, 6)  # Extract "YY" for context if needed
    
    # Adjust the month
    adjusted_month <- month - 1
    if (adjusted_month == 0) {
      adjusted_month <- 12  # December of the previous year
    }
    
    # Create a descriptive dataframe name
    df_name <- paste0(layer, "_", month.abb[adjusted_month])
    
    # Read the CSV into a dataframe
    df <- read.csv(file, header = TRUE)
    
    # Store the dataframe in the list
    data_list[[df_name]] <- df
    
    # assign to the global environment
    assign(df_name, df, envir = .GlobalEnv)
    
  }
  
  # Return the list of dataframes
  return(data_list)
}
# Use the function to open all the CSVs-----------------------------------------
oyster_farm <- read_and_rename_csvs("/Users/ilanajacobs/SURFO/oysterfarm_all_months")


# Step 2: Write a function combining the data into 1 CSV -----------------------

# We need to combine the full time series into one CSV that we can open later.
combine_csvs <- function(folder_path, output_file) {
  # List all CSV files in the folder
  file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize a list to store the dataframes
  data_list <- list()
  
  if (length(file_list) == 0) {
    stop("No CSV files found in the specified folder.")
  }
  
  # Track all unique column names
  all_columns <- NULL
  
  for (file in file_list) {
    # Extract the base name (without the folder path)
    file_name <- basename(file)
    
    # Parse the file name to extract "surface/bottom" and the date
    parts <- strsplit(file_name, "_")[[1]]
    layer <- tolower(parts[2])  # "surface" or "bottom"
    month <- as.numeric(substr(parts[3], 1, 2))  # Extract "MM" from "MMDDYY"
    year <- substr(parts[3], 5, 6)  # Extract "YY" for context if needed
    
    # Adjust the month
    adjusted_month <- month - 1
    if (adjusted_month == 0) {
      adjusted_month <- 12  # December of the previous year
    }
    
    # Read the CSV and add metadata columns
    df <- read.csv(file, header = TRUE)
    df$layer <- layer
    df$month <- month.abb[adjusted_month]
    
    # Update the master list of all columns
    all_columns <- union(all_columns, colnames(df))
    
    # Store the dataframe
    data_list[[file_name]] <- df
  }
  
  # Align all dataframes to have the same columns
  for (name in names(data_list)) {
    df <- data_list[[name]]
    missing_cols <- setdiff(all_columns, colnames(df))
    
    # Add missing columns with NA
    if (length(missing_cols) > 0) {
      df[missing_cols] <- NA
    }
    
    # Reorder columns to match the master set
    data_list[[name]] <- df[, all_columns]
  }
  
  # Combine all dataframes into one
  combined_data <- do.call(rbind, data_list)
  
  # Write the combined dataframe to a new CSV file
  write.csv(combined_data, file = output_file, row.names = TRUE)
  
  # Return the combined dataframe
  return(combined_data)
}

# Step 3: Use the function to combine all the CSVs  -----------------------------
oysterfarm_ts <- combine_csvs("/Users/ilanajacobs/SURFO/AGU_Data/oysterfarm_all_months",
                              "combined_oyster_data.csv")
